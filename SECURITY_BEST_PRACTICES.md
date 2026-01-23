# Security Best Practices Guide

## Table of Contents
1. [Credential Management](#credential-management)
2. [Network Security](#network-security)
3. [Code Security](#code-security)
4. [Deployment Security](#deployment-security)
5. [Monitoring and Auditing](#monitoring-and-auditing)
6. [Incident Response](#incident-response)

---

## 1. Credential Management

### DO ✅

1. **Use Environment Variables**
   ```bash
   # Set environment variables before running
   export ALPACA_API_KEY="your_api_key"
   export ALPACA_SECRET_KEY="your_secret_key"
   export ALPACA_MODE="paper"
   ```

2. **Use Secrets Management Services**
   - **AWS:** AWS Secrets Manager or Parameter Store
   - **Azure:** Azure Key Vault
   - **GCP:** Google Secret Manager
   - **HashiCorp:** Vault

3. **Load Config from Environment**
   ```java
   // Secure: Load from environment
   AlpacaConfig config = AlpacaConfig.fromEnvironment();
   ```

4. **Create .env Files for Development**
   ```bash
   # Copy the example file
   cp .env.example .env
   # Edit .env with your credentials (this file is gitignored)
   ```

5. **Rotate Credentials Regularly**
   - Rotate API keys every 90 days
   - Rotate after any security incident
   - Use separate keys for development/staging/production

### DON'T ❌

1. **Never Hardcode Credentials**
   ```java
   // WRONG - Never do this!
   AlpacaConfig config = new AlpacaConfig(
       "AKXXXXXXXXXX",  // ❌ Hardcoded API key
       "secretXXXXXXXX", // ❌ Hardcoded secret
       baseUrl, dataUrl
   );
   ```

2. **Never Commit Credentials to Git**
   ```bash
   # Make sure .env is in .gitignore
   git add .env  # ❌ NEVER DO THIS
   ```

3. **Never Log Credentials**
   ```java
   // WRONG - This would log the secret!
   logger.info("Config: " + config);  // ❌ Without toString override
   
   // RIGHT - toString() redacts secrets
   logger.info("Config: " + config);  // ✅ Prints "***REDACTED***"
   ```

4. **Never Share Credentials in Plain Text**
   - Don't send via email, Slack, or SMS
   - Don't store in wikis or documentation
   - Use secure sharing tools (1Password, LastPass, etc.)

---

## 2. Network Security

### TLS/SSL Encryption

#### For Production Servers

1. **Generate SSL Certificates**
   ```bash
   # Self-signed for testing only
   openssl req -x509 -newkey rsa:4096 -keyout key.pem -out cert.pem -days 365
   
   # For production, use Let's Encrypt or commercial CA
   ```

2. **Add SSL to Netty Server** (Future Enhancement)
   ```java
   // Add SSL handler to pipeline
   SslContext sslContext = SslContextBuilder
       .forServer(new File("cert.pem"), new File("key.pem"))
       .build();
   pipeline.addLast(sslContext.newHandler(ch.alloc()));
   ```

3. **Use Strong Cipher Suites**
   ```java
   SslContextBuilder.forServer(certFile, keyFile)
       .ciphers(Http2SecurityUtil.CIPHERS, SupportedCipherSuiteFilter.INSTANCE)
       .applicationProtocolConfig(alpnConfig)
       .build();
   ```

### Authentication and Authorization

1. **Implement API Key Authentication**
   ```java
   // Validate client API key on connection
   String clientApiKey = request.getHeader("X-API-Key");
   if (!isValidApiKey(clientApiKey)) {
       throw new UnauthorizedException("Invalid API key");
   }
   ```

2. **IP Whitelisting**
   ```java
   // Check if client IP is allowed
   String clientIp = ctx.channel().remoteAddress().toString();
   if (!allowedIps.contains(clientIp)) {
       ctx.close();
   }
   ```

3. **Rate Limiting**
   ```java
   // Implement rate limiting per client
   if (rateLimiter.tryAcquire()) {
       processRequest();
   } else {
       throw new TooManyRequestsException();
   }
   ```

---

## 3. Code Security

### Input Validation

1. **Validate All External Input**
   ```java
   public void processOrder(String orderJson) {
       // Validate JSON structure
       if (orderJson == null || orderJson.length() > MAX_ORDER_SIZE) {
           throw new InvalidInputException("Invalid order data");
       }
       
       // Parse and validate
       Order order = gson.fromJson(orderJson, Order.class);
       if (!isValidOrder(order)) {
           throw new InvalidInputException("Order validation failed");
       }
   }
   ```

2. **Sanitize String Inputs**
   ```java
   // Remove potentially dangerous characters
   String sanitized = input.replaceAll("[^a-zA-Z0-9_-]", "");
   ```

3. **Use Parameterized Queries** (if using SQL)
   ```java
   // RIGHT - Parameterized query
   String sql = "SELECT * FROM orders WHERE id = ?";
   PreparedStatement stmt = conn.prepareStatement(sql);
   stmt.setString(1, orderId);
   
   // WRONG - SQL injection risk
   String sql = "SELECT * FROM orders WHERE id = '" + orderId + "'";  // ❌
   ```

### Safe Deserialization

1. **Validate JSON Schema**
   ```java
   // Use JSON schema validation
   JsonSchema schema = JsonSchemaFactory.getInstance().getSchema(schemaFile);
   schema.validate(jsonNode);
   ```

2. **Limit Object Graph Depth**
   ```java
   GsonBuilder builder = new GsonBuilder()
       .setLenient()
       .setDateFormat("yyyy-MM-dd'T'HH:mm:ssX");
   // Consider adding custom deserializers for complex types
   ```

### Error Handling

1. **Don't Expose Internal Details**
   ```java
   // WRONG - Exposes stack trace
   catch (Exception e) {
       return "Error: " + e.getMessage() + "\n" + e.getStackTrace();  // ❌
   }
   
   // RIGHT - Generic error message
   catch (Exception e) {
       logger.error("Order processing failed", e);  // Log internally
       return "Error processing order. Request ID: " + requestId;  // Generic message
   }
   ```

2. **Log Security Events**
   ```java
   // Log authentication failures
   logger.warn("Failed authentication attempt from IP: {}", clientIp);
   
   // Log suspicious activity
   logger.warn("Potential DoS attack detected from IP: {}", clientIp);
   ```

---

## 4. Deployment Security

### Development Environment

1. **Use Paper Trading**
   ```bash
   export ALPACA_MODE=paper
   ```

2. **Separate Development Credentials**
   - Use different API keys for dev/staging/production
   - Never use production keys in development

### Production Environment

1. **Use Secret Management**
   ```bash
   # AWS Example
   aws secretsmanager get-secret-value --secret-id alpaca/api-keys
   
   # Azure Example
   az keyvault secret show --name alpaca-api-key --vault-name my-vault
   ```

2. **Enable Firewall Rules**
   ```bash
   # Only allow specific IPs
   iptables -A INPUT -p tcp --dport 8080 -s 192.168.1.0/24 -j ACCEPT
   iptables -A INPUT -p tcp --dport 8080 -j DROP
   ```

3. **Use Container Security**
   ```dockerfile
   # Run as non-root user
   RUN useradd -r -u 1000 hftuser
   USER hftuser
   
   # Don't include secrets in image
   # Use environment variables or volume mounts
   ```

### Infrastructure Security

1. **Network Segmentation**
   - Place trading servers in private subnet
   - Use VPN for remote access
   - Implement network ACLs

2. **Enable Monitoring**
   - CPU/Memory usage alerts
   - Network traffic monitoring
   - Failed authentication alerts
   - Unusual trading patterns

---

## 5. Monitoring and Auditing

### Logging Best Practices

1. **What to Log**
   ```java
   // Log security events
   logger.info("User authenticated: {}", userId);
   logger.warn("Failed login attempt: {}", username);
   logger.error("Unauthorized access attempt: {}", endpoint);
   
   // Log business events
   logger.info("Order placed: {} shares of {} at ${}", qty, symbol, price);
   logger.info("Trade executed: {}", tradeId);
   ```

2. **What NOT to Log**
   ```java
   // NEVER log credentials
   logger.debug("API Key: {}", apiKey);  // ❌ NEVER
   logger.debug("Secret: {}", secret);    // ❌ NEVER
   logger.debug("Password: {}", pwd);     // ❌ NEVER
   
   // Don't log full credit card numbers
   logger.debug("Card: {}", ccNumber);    // ❌ NEVER
   ```

3. **Structured Logging**
   ```java
   // Use structured logging with MDC
   MDC.put("requestId", requestId);
   MDC.put("userId", userId);
   logger.info("Processing order");
   // Output: [requestId=123 userId=456] Processing order
   ```

### Audit Trails

1. **Use Ada Audit System**
   - All compliance checks are logged
   - Merkle trees ensure tamper-evidence
   - Export audit logs regularly

2. **Track Critical Events**
   - User authentication/authorization
   - Configuration changes
   - Trading activities
   - System errors and exceptions

---

## 6. Incident Response

### Security Incident Checklist

1. **Detect and Contain**
   - [ ] Identify the security incident
   - [ ] Isolate affected systems
   - [ ] Preserve logs and evidence

2. **Immediate Actions**
   - [ ] Rotate all API keys and credentials
   - [ ] Review access logs for suspicious activity
   - [ ] Check for unauthorized transactions
   - [ ] Notify relevant stakeholders

3. **Investigation**
   - [ ] Analyze logs for root cause
   - [ ] Determine scope of breach
   - [ ] Document timeline of events

4. **Remediation**
   - [ ] Apply security patches
   - [ ] Update security controls
   - [ ] Implement additional monitoring

5. **Recovery**
   - [ ] Restore systems from clean backups
   - [ ] Verify system integrity
   - [ ] Resume normal operations

6. **Post-Incident**
   - [ ] Conduct post-mortem analysis
   - [ ] Update security procedures
   - [ ] Provide training if needed

### Emergency Contacts

```text
Security Team: security@yourcompany.com
On-Call Engineer: +1-XXX-XXX-XXXX
Alpaca Support: https://alpaca.markets/support
```

---

## 7. Compliance Requirements

### Financial Services Compliance

1. **Audit Trails**
   - Maintain complete audit logs for all transactions
   - Keep logs for minimum retention period (usually 7 years)
   - Ensure logs are tamper-evident

2. **Data Protection**
   - Encrypt data at rest and in transit
   - Implement access controls
   - Regular security assessments

3. **Reporting**
   - Implement transaction reporting mechanisms
   - Maintain compliance with SEC/FINRA regulations
   - Ensure MiFID II compliance if applicable

---

## 8. Security Checklist

### Before Every Deployment

- [ ] All credentials loaded from secure sources (not hardcoded)
- [ ] .env files not committed to Git
- [ ] TLS/SSL enabled for all network communication
- [ ] Authentication implemented for all endpoints
- [ ] Input validation in place
- [ ] Rate limiting configured
- [ ] Logging properly configured (no secrets logged)
- [ ] Security headers added to responses
- [ ] Dependencies scanned for vulnerabilities
- [ ] Code reviewed for security issues
- [ ] Penetration testing completed
- [ ] Incident response plan in place

### Regular Security Tasks

**Daily:**
- Monitor logs for suspicious activity
- Check system health and alerts

**Weekly:**
- Review failed authentication attempts
- Analyze security metrics
- Update threat intelligence

**Monthly:**
- Scan dependencies for vulnerabilities
- Review and update access controls
- Test backup and recovery procedures

**Quarterly:**
- Rotate credentials
- Conduct security training
- Review and update security policies
- Perform security assessments

---

## 9. Additional Resources

### Security Tools

- **Dependency Scanning:** GitHub Dependabot, Snyk, OWASP Dependency-Check
- **Static Analysis:** SonarQube, CodeQL, SpotBugs
- **Dynamic Analysis:** OWASP ZAP, Burp Suite
- **Secret Scanning:** git-secrets, TruffleHog, GitHub Secret Scanning

### Security References

- OWASP Top 10: https://owasp.org/www-project-top-ten/
- NIST Cybersecurity Framework: https://www.nist.gov/cyberframework
- CWE Top 25: https://cwe.mitre.org/top25/
- SEC Cybersecurity Guidance: https://www.sec.gov/

### Training Resources

- OWASP WebGoat (hands-on security training)
- PortSwigger Web Security Academy
- SANS Secure Coding training
- Alpaca API Security Best Practices

---

## Conclusion

Security is not a one-time task but an ongoing process. Regular review and updates of security practices are essential for maintaining a secure trading system. Always stay informed about new vulnerabilities and security best practices in the financial technology sector.

**Remember:** When in doubt, consult with a security professional before deploying to production.

---

**Last Updated:** 2026-01-23  
**Version:** 1.0
