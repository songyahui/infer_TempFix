#define SW_CHANNEL_MIN_MEM (1024*64)

/*@ return(arg):
    Post (TRUE, return(arg))@*/

/*@ ERR_new():
    Post (TRUE, ERR_new())@*/

/*@ SSL_do_handshake(s):
    Future ((ret=-1), return(ret))@*/

/*@ i2a_ASN1_OBJECT(bp, a):
    Future ((ret=-1), return(ret))@*/

/*@ BIO_new_NDEF(out, val, it):
    Future ((ret=0), return(ret))@*/

/*@ BIO_ADDR_new():
    Future ((ret=0), return(ret))@*/

/*@ BIO_parse_hostserv(hostserv, host, service, hostserv_prio):
    Future ((ret=0), return(ret))@*/

/*@ BIO_lookup_ex(host, service, lookup_type, family, socktype, protocol, res):
    Future ((ret=0), return(ret))@*/

/*@ BIO_new_ex(libctx, method):
    Future ((ret=0), return(ret))@*/

/*@ BIO_new(method):
    Future ((ret=0), return(ret))@*/

/*@ BIO_gets(b, buf, size):
    Future ((ret=-1), return(ret))@*/

/*@ BIO_get_line(bio, buf, size):
    Future ((ret=-1), return(ret))@*/

/*@ BIO_ctrl(b, cmd, larg, parg):
    Future ((ret=-2), return(ret))@*/

/*@ BIO_callback_ctrl(b, cmd, fp):
    Future ((ret=-2), return(ret))@*/

/*@ BIO_find_type(bio, type):
    Future ((ret=0), return(ret))@*/

/*@ BIO_get_new_index():
    Future ((ret=-1), return(ret))@*/

/*@ BIO_meth_new(type, name):
    Future ((ret=0), return(ret))@*/

/*@ BIO_sock_info(sock, type, info):
    Future ((ret=0), return(ret))@*/

/*@ BIO_socket(domain, socktype, protocol, options):
    Future ((ret=-1), return(ret))@*/

/*@ BIO_connect(sock, addr, options):
    Future ((ret=0), return(ret))@*/

/*@ BIO_bind(sock, addr, options):
    Future ((ret=0), return(ret))@*/

/*@ BIO_listen(sock, addr, options):
    Future ((ret=0), return(ret))@*/

/*@ BIO_accept_ex(accept_sock, addr_, options):
    Future ((ret=-1), return(ret))@*/

/*@ BIO_ACCEPT_new():
    Future ((ret=0), return(ret))@*/

/*@ BIO_ctrl_get_write_guarantee(bio):
    Future ((ret=-2), return(ret))@*/

/*@ BIO_ctrl_get_read_request(bio):
    Future ((ret=-2), return(ret))@*/

/*@ BIO_nread0(bio, buf):
    Future ((ret=-2), return(ret))@*/

/*@ BIO_nread(bio, buf, num):
    Future ((ret=-2), return(ret))@*/

/*@ BIO_nwrite0(bio, buf):
    Future ((ret=-2), return(ret))@*/

/*@ BIO_nwrite(bio, buf, num):
    Future ((ret=-2), return(ret))@*/

/*@ BIO_CONNECT_new():
    Future ((ret=0), return(ret))@*/

/*@ BIO_new_file(filename, mode):
    Future ((ret=0), return(ret))@*/

/*@ BIO_new_mem_buf(buf, len):
    Future ((ret=0), return(ret))@*/

/*@ BN_usub(r, a, b):
    Future ((ret=0), return(ret))@*/

/*@ BN_BLINDING_new(A, Ai, mod):
    Future ((ret=0), return(ret))@*/

/*@ BN_BLINDING_convert_ex(n, r, b, ctx):
    Future ((ret=0), return(ret))@*/

/*@ BN_BLINDING_invert_ex(n, r, b, ctx):
    Future ((ret=0), return(ret))@*/

/*@ BN_bn2dec(a):
    Future ((ret=0), return(ret))@*/

/*@ BN_hex2bn(bn, a):
    Future ((ret=0), return(ret))@*/

/*@ BN_CTX_new_ex(ctx):
    Future ((ret=0), return(ret))@*/

/*@ BN_CTX_new():
    Future ((ret=0), return(ret))@*/

/*@ BN_CTX_get(ctx):
    Future ((ret=0), return(ret))@*/

/*@ BN_STACK_push(st, idx):
    Future ((ret=0), return(ret))@*/

/*@ BN_POOL_get(p, flag):
    Future ((ret=0), return(ret))@*/

/*@ BN_div(dv, rm, num, divisor, ctx):
    Future ((ret=0), return(ret))@*/

/*@ BN_exp(r, a, p, ctx):
    Future ((ret=0), return(ret))@*/

/*@ BN_mod_exp_recp(r, a, p, m, ctx):
    Future ((ret=0), return(ret))@*/

/*@ BN_mod_exp_mont(rr, a, p, m, ctx, in_mont):
    Future ((ret=0), return(ret))@*/

/*@ BN_mod_exp_mont_word(rr, a, p, m, ctx, in_mont):
    Future ((ret=0), return(ret))@*/

/*@ BN_mod_exp_simple(r, a, p, m, ctx):
    Future ((ret=0), return(ret))@*/

/*@ BN_mod_exp2_mont(rr, a1, p1, a2, p2, m, ctx, in_mont):
    Future ((ret=0), return(ret))@*/

/*@ BN_GF2m_mod(r, a, p):
    Future ((ret=0), return(ret))@*/

/*@ BN_new():
    Future ((ret=0), return(ret))@*/

/*@ BN_GENCB_new():
    Future ((ret=0), return(ret))@*/

/*@ BN_mod_sqr(r, a, m, ctx):
    Future ((ret=0), return(ret))@*/

/*@ BN_mod_lshift_quick(r, a, n, m):
    Future ((ret=0), return(ret))@*/

/*@ BN_MONT_CTX_new():
    Future ((ret=0), return(ret))@*/

/*@ BN_mpi2bn(d, n, ain):
    Future ((ret=0), return(ret))@*/

/*@ BN_generate_prime_ex2(ret, bits, safe, add, rem, cb, ctx):
    Future ((ret=0), return(ret))@*/

/*@ BN_RECP_CTX_new():
    Future ((ret=0), return(ret))@*/

/*@ BN_lshift(r, a, n):
    Future ((ret=0), return(ret))@*/

/*@ BN_rshift(r, a, n):
    Future ((ret=0), return(ret))@*/

/*@ BIO_new_CMS(out, cms):
    Future ((ret=0), return(ret))@*/

/*@ BIO_new_PKCS7(out, p7):
    Future ((ret=0), return(ret))@*/

/*@ SSL_dup_CA_list(sk):
    Future ((ret=0), return(ret))@*/

/*@ SSL_CIPHER_description(cipher, buf, len):
    Future ((ret=0), return(ret))@*/

/*@ SSL_COMP_add_compression_method(id, cm):
    Future ((ret=1), return(ret))@*/

/*@ SSL_CONF_cmd(cctx, cmd, value):
    Future ((ret=-2), return(ret))@*/

/*@ SSL_clear(s):
    Future ((ret=0), return(ret))@*/

/*@ SSL_CTX_set_ssl_version(ctx, meth):
    Future ((ret=0), return(ret))@*/

/*@ SSL_new(ctx):
    Future ((ret=0), return(ret))@*/

/*@ SSL_CTX_set_session_id_context(ctx, sid_ctx, sid_ctx_len):
    Future ((ret=0), return(ret))@*/

/*@ SSL_set_session_id_context(ssl, sid_ctx, sid_ctx_len):
    Future ((ret=0), return(ret))@*/

/*@ SSL_dane_enable(s, basedomain):
    Future ((ret=-1), return(ret))@*/

/*@ SSL_set_wfd(s, fd):
    Future ((ret=0), return(ret))@*/

/*@ SSL_set_rfd(s, fd):
    Future ((ret=0), return(ret))@*/

/*@ SSL_CTX_check_private_key(ctx):
    Future ((ret=0), return(ret))@*/

/*@ SSL_check_private_key(ssl):
    Future ((ret=0), return(ret))@*/

/*@ SSL_accept(s):
    Future ((ret=-1), return(ret))@*/

/*@ SSL_connect(s):
    Future ((ret=-1), return(ret))@*/

/*@ SSL_read(s, buf, num):
    Future ((ret=-1), return(ret))@*/

/*@ SSL_read_early_data(s, buf, num, readbytes):
    Future ((ret=0), return(ret))@*/

/*@ SSL_peek(s, buf, num):
    Future ((ret=-1), return(ret))@*/

/*@ SSL_sendfile(s, fd, offset, size, flags):
    Future ((ret=-1), return(ret))@*/

/*@ SSL_write(s, buf, num):
    Future ((ret=-1), return(ret))@*/

/*@ SSL_write_early_data(s, buf, num, written):
    Future ((ret=0), return(ret))@*/

/*@ SSL_shutdown(s):
    Future ((ret=-1), return(ret))@*/

/*@ SSL_key_update(s, updatetype):
    Future ((ret=0), return(ret))@*/

/*@ SSL_CTX_set_cipher_list(ctx, str):
    Future ((ret=0), return(ret))@*/

/*@ SSL_set_cipher_list(s, str):
    Future ((ret=0), return(ret))@*/

/*@ SSL_CTX_set_alpn_protos(ctx, protos, protos_len):
    Future ((ret=1), return(ret))@*/

/*@ SSL_set_alpn_protos(ssl, protos, protos_len):
    Future ((ret=1), return(ret))@*/

/*@ SSL_CTX_new_ex(libctx, propq, meth):
    Future ((ret=0), return(ret))@*/

/*@ SSL_CTX_new(meth):
    Future ((ret=0), return(ret))@*/

/*@ SSL_CTX_use_psk_identity_hint(ctx, identity_hint):
    Future ((ret=0), return(ret))@*/

/*@ SSL_use_psk_identity_hint(s, identity_hint):
    Future ((ret=0), return(ret))@*/

/*@ SSL_set_ct_validation_callback(s, callback, arg):
    Future ((ret=0), return(ret))@*/

/*@ SSL_CTX_set_ct_validation_callback(ctx, callback, arg):
    Future ((ret=0), return(ret))@*/

/*@ SSL_CTX_enable_ct(ctx, validation_mode):
    Future ((ret=0), return(ret))@*/

/*@ SSL_enable_ct(s, validation_mode):
    Future ((ret=0), return(ret))@*/

/*@ SSL_client_hello_get1_extensions_present(s, out, outlen):
    Future ((ret=0), return(ret))@*/

/*@ SSL_verify_client_post_handshake(ssl):
    Future ((ret=0), return(ret))@*/

/*@ SSL_set0_tmp_dh_pkey(s, dhpkey):
    Future ((ret=0), return(ret))@*/

/*@ SSL_CTX_set0_tmp_dh_pkey(ctx, dhpkey):
    Future ((ret=0), return(ret))@*/

/*@ SSL_use_certificate(ssl, x):
    Future ((ret=0), return(ret))@*/

/*@ SSL_use_certificate_ASN1(ssl, d, len):
    Future ((ret=0), return(ret))@*/

/*@ SSL_use_PrivateKey(ssl, pkey):
    Future ((ret=0), return(ret))@*/

/*@ SSL_use_PrivateKey_ASN1(type, ssl, d, len):
    Future ((ret=0), return(ret))@*/

/*@ SSL_CTX_use_certificate(ctx, x):
    Future ((ret=0), return(ret))@*/

/*@ SSL_CTX_use_certificate_ASN1(ctx, len, d):
    Future ((ret=0), return(ret))@*/

/*@ SSL_CTX_use_PrivateKey(ctx, pkey):
    Future ((ret=0), return(ret))@*/

/*@ SSL_CTX_use_PrivateKey_ASN1(type, ctx, d, len):
    Future ((ret=0), return(ret))@*/

/*@ SSL_CTX_use_serverinfo_ex(ctx, version, serverinfo, serverinfo_length):
    Future ((ret=0), return(ret))@*/

/*@ SSL_CTX_use_serverinfo(ctx, serverinfo, serverinfo_length):
    Future ((ret=0), return(ret))@*/

/*@ SSL_use_RSAPrivateKey(ssl, rsa):
    Future ((ret=0), return(ret))@*/

/*@ SSL_use_RSAPrivateKey_ASN1(ssl, d, len):
    Future ((ret=0), return(ret))@*/

/*@ SSL_CTX_use_RSAPrivateKey(ctx, rsa):
    Future ((ret=0), return(ret))@*/

/*@ SSL_CTX_use_RSAPrivateKey_ASN1(ctx, d, len):
    Future ((ret=0), return(ret))@*/

/*@ SSL_SESSION_new():
    Future ((ret=0), return(ret))@*/

/*@ SSL_SESSION_set1_id(s, sid, sid_len):
    Future ((ret=0), return(ret))@*/

/*@ SSL_SESSION_set1_id_context(s, sid_ctx, sid_ctx_len):
    Future ((ret=0), return(ret))@*/

/*@ SSL_set_session_ticket_ext(s, ext_data, ext_len):
    Future ((ret=0), return(ret))@*/

/*@ SSL_SESSION_print_fp(fp, x):
    Future ((ret=0), return(ret))@*/

/*@ SSL_CTX_set_tlsext_max_fragment_length(ctx, mode):
    Future ((ret=0), return(ret))@*/

/*@ SSL_set_tlsext_max_fragment_length(ssl, mode):
    Future ((ret=0), return(ret))@*/

/*@ SSL_CTX_set_client_cert_engine(ctx, e):
    Future ((ret=0), return(ret))@*/

/*@ BIO_lookup(host, service, lookup_type, family, socktype, res):
    Future ((ret=0), return(ret))@*/

/*@ BIO_int_ctrl(b, cmd, larg, iarg):
    Future ((ret=-2), return(ret))@*/

/*@ BN_BLINDING_convert(n, b, ctx):
    Future ((ret=0), return(ret))@*/

/*@ BN_BLINDING_invert(n, b, ctx):
    Future ((ret=0), return(ret))@*/

