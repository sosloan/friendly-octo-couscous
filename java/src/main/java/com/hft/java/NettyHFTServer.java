package com.hft.java;

import io.netty.bootstrap.ServerBootstrap;
import io.netty.channel.*;
import io.netty.channel.nio.NioEventLoopGroup;
import io.netty.channel.socket.SocketChannel;
import io.netty.channel.socket.nio.NioServerSocketChannel;
import io.netty.handler.codec.string.StringDecoder;
import io.netty.handler.codec.string.StringEncoder;
import io.netty.handler.logging.LogLevel;
import io.netty.handler.logging.LoggingHandler;

import java.util.logging.Logger;

/**
 * High-Performance Netty-based HFT Network Server
 * Provides ultra-low latency network communication for trading
 */
public class NettyHFTServer {
    
    private static final Logger logger = Logger.getLogger(NettyHFTServer.class.getName());
    private final int port;
    
    public NettyHFTServer(int port) {
        this.port = port;
    }
    
    public void start() throws Exception {
        // Boss group handles incoming connections
        EventLoopGroup bossGroup = new NioEventLoopGroup(1);
        // Worker group handles traffic of accepted connections
        EventLoopGroup workerGroup = new NioEventLoopGroup();
        
        try {
            ServerBootstrap bootstrap = new ServerBootstrap();
            bootstrap.group(bossGroup, workerGroup)
                .channel(NioServerSocketChannel.class)
                .option(ChannelOption.SO_BACKLOG, 1024)
                .option(ChannelOption.SO_REUSEADDR, true)
                .childOption(ChannelOption.TCP_NODELAY, true) // Critical for low latency
                .childOption(ChannelOption.SO_KEEPALIVE, true)
                .handler(new LoggingHandler(LogLevel.INFO))
                .childHandler(new ChannelInitializer<SocketChannel>() {
                    @Override
                    protected void initChannel(SocketChannel ch) {
                        ChannelPipeline pipeline = ch.pipeline();
                        pipeline.addLast(new StringDecoder());
                        pipeline.addLast(new StringEncoder());
                        pipeline.addLast(new HFTServerHandler());
                    }
                });
            
            // Bind and start accepting connections
            ChannelFuture future = bootstrap.bind(port).sync();
            logger.info("🚀 Netty HFT Server started on port " + port);
            logger.info("⚡ TCP_NODELAY enabled for ultra-low latency");
            
            // Wait until server socket is closed
            future.channel().closeFuture().sync();
        } finally {
            bossGroup.shutdownGracefully();
            workerGroup.shutdownGracefully();
        }
    }
    
    /**
     * Handler for HFT messages
     */
    private static class HFTServerHandler extends SimpleChannelInboundHandler<String> {
        
        @Override
        protected void channelRead0(ChannelHandlerContext ctx, String msg) {
            logger.info("Received order: " + msg);
            
            // Echo back with acknowledgment
            String response = "ACK: " + msg + " processed\n";
            ctx.writeAndFlush(response);
        }
        
        @Override
        public void channelActive(ChannelHandlerContext ctx) {
            logger.info("Client connected: " + ctx.channel().remoteAddress());
            ctx.writeAndFlush("Welcome to HFT Netty Server\n");
        }
        
        @Override
        public void exceptionCaught(ChannelHandlerContext ctx, Throwable cause) {
            logger.severe("Exception: " + cause.getMessage());
            ctx.close();
        }
    }
}
