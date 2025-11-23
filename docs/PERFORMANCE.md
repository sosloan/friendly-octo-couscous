# Performance Tuning Guide

## Overview

This guide provides recommendations for optimizing each component of the HFT system for maximum performance.

## Ada Performance Optimization

### Compiler Flags
```bash
gprbuild -P hft.gpr -XBuild=Production -cargs -O3 -gnatn -funroll-loops
```

**Key Flags:**
- `-O3` - Maximum optimization
- `-gnatn` - Enable inlining
- `-funroll-loops` - Unroll loops for performance

### Real-Time Configuration
```ada
pragma Priority (System.Priority'Last);  -- Highest priority
pragma CPU (1);                          -- Pin to CPU 1
```

### Memory Management
- Use stack allocation where possible
- Avoid dynamic allocation in critical paths
- Pre-allocate buffers for orders

## Lean Performance

### Build Optimization
```bash
lake build --release
```

### Theorem Caching
- Lean caches compiled proofs
- Keep `.lake/build` directory for fast rebuilds
- Use `import` instead of redefining common theorems

## Akka Performance Tuning

### Dispatcher Configuration
```hocon
akka.actor.default-dispatcher {
  type = Dispatcher
  executor = "fork-join-executor"
  
  fork-join-executor {
    parallelism-min = 16      # Min threads
    parallelism-factor = 4.0  # Threads per CPU
    parallelism-max = 128     # Max threads
  }
  
  throughput = 100            # Messages per actor before switch
}
```

### Mailbox Tuning
```hocon
bounded-mailbox {
  mailbox-type = "akka.dispatch.BoundedMailbox"
  mailbox-capacity = 10000
  mailbox-push-timeout-time = 10ms
}
```

### JVM Flags
```bash
-XX:+UseG1GC                    # G1 garbage collector
-XX:MaxGCPauseMillis=10         # Max GC pause
-XX:+UseStringDeduplication     # Reduce memory
-Xms4G -Xmx4G                   # Fixed heap size
```

## Java/Netty Performance

### JVM Optimization
```bash
java \
  -XX:+UseZGC \                 # ZGC for low latency
  -XX:+UseNUMA \                # NUMA-aware
  -XX:+AlwaysPreTouch \         # Pre-touch memory
  -XX:+UseLargePages \          # Use huge pages
  --enable-preview \            # Virtual threads
  -Xms8G -Xmx8G                 # Fixed heap
  -jar hft.jar
```

### Netty Configuration
```java
ServerBootstrap bootstrap = new ServerBootstrap()
    .option(ChannelOption.SO_BACKLOG, 1024)
    .option(ChannelOption.SO_REUSEADDR, true)
    .childOption(ChannelOption.TCP_NODELAY, true)    // Critical!
    .childOption(ChannelOption.SO_KEEPALIVE, true)
    .childOption(ChannelOption.SO_RCVBUF, 256 * 1024)
    .childOption(ChannelOption.SO_SNDBUF, 256 * 1024)
    .childOption(ChannelOption.ALLOCATOR, PooledByteBufAllocator.DEFAULT);
```

### Virtual Threads
```java
// Use virtual threads for I/O-bound tasks
Thread.ofVirtual().start(() -> {
    // Network I/O, blocking operations
});

// Use platform threads for CPU-bound tasks
Thread.ofPlatform().start(() -> {
    // Calculations, data processing
});
```

## Erlang/OTP Performance

### VM Arguments
```bash
erl \
  +S 16:16 \              # 16 schedulers, 16 cores
  +K true \               # Enable kernel poll
  +A 128 \                # 128 async threads
  +sbwt very_long \       # Busy wait for schedulers
  +swt very_low \         # Wake threads immediately
  +zdbbl 32768            # Larger dist buffer
```

### Process Configuration
```erlang
% Spawn with higher priority
spawn_opt(Fun, [
    {priority, high},
    {min_heap_size, 10000},
    {fullsweep_after, 0}
]).
```

### ETS Tables
```erlang
% Use ETS for shared state
ets:new(orders, [
    set,
    named_table,
    public,
    {read_concurrency, true},
    {write_concurrency, true},
    {decentralized_counters, true}
]).
```

## System-Level Optimization

### Linux Kernel Tuning

#### Network Stack
```bash
# /etc/sysctl.conf
net.core.rmem_max = 134217728
net.core.wmem_max = 134217728
net.core.netdev_max_backlog = 5000
net.ipv4.tcp_rmem = 4096 87380 134217728
net.ipv4.tcp_wmem = 4096 65536 134217728
net.ipv4.tcp_congestion_control = bbr
net.ipv4.tcp_fastopen = 3
```

#### CPU Isolation
```bash
# Isolate CPUs for HFT processes
# Add to kernel boot parameters
isolcpus=2-7 nohz_full=2-7 rcu_nocbs=2-7
```

#### Huge Pages
```bash
# Enable huge pages
echo 2048 > /proc/sys/vm/nr_hugepages

# Mount hugetlbfs
mount -t hugetlbfs none /mnt/huge
```

### Real-Time Configuration

#### PREEMPT_RT Kernel
```bash
# Use PREEMPT_RT kernel for deterministic latency
uname -a | grep PREEMPT_RT
```

#### Process Priority
```bash
# Set real-time priority
chrt -f 99 ./hft_main    # FIFO scheduling, priority 99
```

#### CPU Affinity
```bash
# Pin to specific CPU
taskset -c 2 ./hft_main  # Run on CPU 2
```

## Monitoring & Profiling

### Performance Metrics

#### Ada
```bash
gprof hft_main gmon.out  # Profile with gprof
```

#### Java
```bash
# JFR (Java Flight Recorder)
java -XX:StartFlightRecording=filename=recording.jfr ...

# Async-profiler
./profiler.sh -d 60 -f flamegraph.html <pid>
```

#### Erlang
```erlang
% Percept profiling
percept:profile("profile.dat", {Module, Function, Args}).
percept:analyze("profile.dat").
```

### Latency Measurement

```bash
# Measure network latency
ping -i 0.001 <target>

# Measure syscall latency
strace -T -e trace=network ./hft_main

# Perf tools
perf stat -e cycles,instructions,cache-references,cache-misses ./hft_main
```

## Benchmark Results

### Expected Performance (Tuned System)

| Operation | P50 | P99 | P99.9 |
|-----------|-----|-----|-------|
| Order Validation (Ada) | 0.5 μs | 1 μs | 2 μs |
| Order Matching (Erlang) | 5 μs | 15 μs | 30 μs |
| Network RTT (Netty) | 50 μs | 100 μs | 200 μs |
| End-to-End Order | 100 μs | 300 μs | 500 μs |

### Throughput

| Component | Throughput |
|-----------|------------|
| Ada Engine | 2M orders/sec |
| Akka Actors | 5M messages/sec |
| Netty Server | 100K connections |
| Erlang Supervisor | 1M processes |

## Best Practices

1. **Profile First** - Measure before optimizing
2. **Avoid Allocation** - Minimize GC pressure
3. **Batch Operations** - Reduce syscalls
4. **Cache Locality** - Keep data hot in CPU cache
5. **Lock-Free** - Use atomic operations where possible
6. **Pre-allocate** - Avoid runtime allocation
7. **Pin Threads** - Use CPU affinity
8. **Huge Pages** - Reduce TLB misses
9. **NUMA-Aware** - Allocate on local node
10. **Monitor Always** - Continuous performance tracking

## Troubleshooting

### High Latency
- Check GC pauses: `-Xlog:gc*`
- Verify CPU affinity: `taskset -cp <pid>`
- Check context switches: `pidstat -w 1`

### Low Throughput
- Profile with perf: `perf record -g`
- Check thread count: `pstree -p`
- Monitor queue depths: Application metrics

### Memory Issues
- Heap dumps: `jmap -dump:file=heap.bin <pid>`
- Memory maps: `pmap <pid>`
- Leak detection: Valgrind for native code

## Platform-Specific Tuning

### macOS (M1/M2/M3)
- Use unified memory architecture
- Enable Metal for GPU acceleration
- Use GCD for task parallelism

### Linux
- PREEMPT_RT kernel for real-time
- CPU isolation and pinning
- Huge pages for memory

### Windows
- Use IOCP for networking
- Enable High Performance mode
- Disable CPU throttling
