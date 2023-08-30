#define SW_CHANNEL_MIN_MEM (1024*64)

/*@ open(path):
    Future ((ret>=0), (!close(ret))^* · close(ret) · (_)^*)@*/

/*@ socket(domain, type, protocol):
    Future ((ret>=0), (!close(ret))^* · close(ret) · (_)^*)@*/

/*@ close(handler):
    Post (TRUE, close(handler))@*/

/*@ fopen(path):
    Future ((ret>0), (!fclose(ret))^* · fclose(ret) · (_)^*)@*/

/*@ fdopen(path, b):
    Future ((ret>0), ((!fclose(ret))^* · fclose(ret) · (_)^* \/ (!close(path))^* · close(path) · (_)^*))@*/

/*@ fclose(handler):
    Post (TRUE, fclose(handler))@*/

/*@ endmntent(handler):
    Post (TRUE, fclose(handler))@*/

/*@ fflush(handler):
    Post (TRUE, fclose(handler))@*/

/*@ opendir(path):
    Future ((ret>0), (!closedir(ret))^* · closedir(ret) · (_)^*)@*/

/*@ closedir(handler):
    Post (TRUE, closedir(handler))@*/

/*@ rpl_open(path):
    Future ((ret>=0), (!close(ret))^* · close(ret) · (_)^*)@*/

/*@ fopen_safer(file, mode):
    Future ((!(ret=0)/\(ret>0)), (!fclose(ret))^* · fclose(ret) · (_)^*)@*/

/*@ internal_read_file(filename, length, mode):
    Post ((!(stream=0)/\TRUE), fclose(stream)) \/ ((!(stream=0)/\TRUE), fclose(stream)) \/ (((stream=0)/\TRUE), 𝝐)@*/

/*@ save_cwd(cwd):
    Post ((!((cwd.desc<0))/\TRUE), deref(cwd)) \/ (((cwd.desc<0)/\TRUE), deref(cwd) · deref(cwd)) \/ ((!((cwd.desc<0))/\TRUE), deref(cwd) · deref(cwd)) \/ (((cwd.desc<0)/\TRUE), deref(cwd) · deref(cwd) · deref(cwd))@*/

/*@ free_cwd(cwd):
    Post (TRUE, close(cwd.desc))@*/

/*@ try_file(tmpl, flags):
    Future ((ret>=0), (!close(ret))^* · close(ret) · (_)^*)@*/

/*@ fd_safer(fd):
    Post (TRUE, close(fd))@*/

/*@ pipe_safer(fd):
    Post (TRUE, close(fd_1_))@*/

/*@ cdb_free(cdb):
    Post (TRUE, close(cdb.fd))@*/

/*@ flush_stdout():
    Post (TRUE, fclose(__stdoutp))@*/

/*@ error_tail(status, errnum, message, args):
    Post (TRUE, fclose(__stderrp)) \/ (TRUE, fclose(__stderrp))@*/

/*@ orig_open(filename, flags, mode):
    Future ((ret>=0), (!close(ret))^* · close(ret) · (_)^*)@*/

/*@ rpl_open(filename, flags):
    Post (TRUE, close(fd))@*/

/*@ rpl_openat(dfd, filename, flags):
    Post (TRUE, close(fd))@*/

/*@ openat_proc_name(buf, fd, file):
    Post ((!((proc_self_fd<0))/\TRUE), close(proc_self_fd)) \/ ((!((proc_self_fd<0))/\TRUE), close(proc_self_fd)) \/ ((!((proc_self_fd<0))/\TRUE), close(proc_self_fd)) \/ (((proc_self_fd<0)/\TRUE), 𝝐) \/ (((proc_self_fd<0)/\TRUE), 𝝐) \/ (((proc_self_fd<0)/\TRUE), 𝝐)@*/

/*@ waitdaemon(nochdir, noclose, maxwait):
    Post ((!(!(fd=-1))/\TRUE), close(i)) \/ (((!(fd=-1)/\!((fd>2)))/\TRUE), close(i)) \/ (((!(fd=-1)/\(fd>2))/\TRUE), close(i) · close(fd))@*/

/*@ ttymsg(iov, iovcnt, line, tmout):
    Post ((!((fd<0))/\TRUE), close(fd)) \/ (!((fd<0)), close(fd)) \/ ((!((fd<0))/\TRUE), close(fd)) \/ ((!((fd<0))/\TRUE), close(fd)) \/ (!((fd<0)), close(fd)) \/ (((fd<0)/\TRUE), 𝝐) \/ (((fd<0)/\TRUE), 𝝐)@*/

/*@ fts_close(sp):
    Post (TRUE, close(sp.fts_rfd))@*/

/*@ fts_read(sp):
    Post (TRUE, close(p.fts_symfd)) \/ (TRUE, close(p.fts_symfd)) \/ (TRUE, close(p.fts_symfd))@*/

/*@ fts_children(sp, instr):
    Post ((!((fd<0))/\TRUE), close(fd)) \/ ((!((fd<0))/\TRUE), 𝝐) \/ (((fd<0)/\TRUE), 𝝐)@*/

/*@ parse_file(file_name):
    Post ((!(file=0)/\TRUE), fclose(file)) \/ (((file=0)/\TRUE), fclose(file))@*/

/*@ resolve_name(name):
    Post (TRUE, close(f)) \/ (TRUE, close(f)) \/ (TRUE, close(f)) \/ (TRUE, close(f)) \/ (TRUE, close(f) · close(f)) \/ (TRUE, close(f) · close(f))@*/

/*@ send_file(fd, name, mode):
    Post (TRUE, fclose(file)) \/ (TRUE, fclose(file) · fclose(file)) \/ (TRUE, fclose(file) · fclose(file) · fclose(file)) \/ (TRUE, fclose(file) · fclose(file) · fclose(file) · fclose(file))@*/

/*@ recvfile(fd, name, mode):
    Post (TRUE, fclose(file)) \/ (TRUE, fclose(file) · fclose(file)) \/ (TRUE, fclose(file) · fclose(file) · fclose(file)) \/ (TRUE, fclose(file) · fclose(file) · fclose(file) · fclose(file))@*/

/*@ do_try(trace, hop, max_hops, max_tries):
    Post (TRUE, fclose(__stdoutp) · fclose(__stdoutp)) \/ (TRUE, fclose(__stdoutp))@*/

/*@ trace_init(t, to, type):
    Post (TRUE, deref(t) · deref(t)) \/ (!((t.icmpfd<0)), deref(t) · deref(t) · deref(t) · deref(t)) \/ ((t.icmpfd<0), deref(t) · deref(t) · deref(t) · deref(t)) \/ (!((t.udpfd<0)), deref(t) · deref(t) · deref(t)) \/ ((!((t.udpfd<0))/\!((t.icmpfd<0))), deref(t) · deref(t) · deref(t) · deref(t) · deref(t)) \/ ((!((t.udpfd<0))/\(t.icmpfd<0)), deref(t) · deref(t) · deref(t) · deref(t) · deref(t)) \/ ((t.udpfd<0), deref(t) · deref(t) · deref(t)) \/ (((t.udpfd<0)/\!((t.icmpfd<0))), deref(t) · deref(t) · deref(t) · deref(t) · deref(t)) \/ (((t.udpfd<0)/\(t.icmpfd<0)), deref(t) · deref(t) · deref(t) · deref(t) · deref(t))@*/

/*@ setescape(argc, argv):
    Post (TRUE, fclose(__stdoutp))@*/

/*@ togcrmod():
    Post (TRUE, fclose(__stdoutp))@*/

/*@ status(argc, argv):
    Post (TRUE, fclose(__stdoutp))@*/

/*@ cmdrc(m1, m2):
    Post (TRUE, fclose(rcfile))@*/

/*@ NetClose(fd):
    Post (TRUE, close(fd))@*/

/*@ SetNetTrace(file):
    Post (TRUE, fclose(NetTrace)) \/ (TRUE, fclose(NetTrace))@*/

/*@ Dump(direction, buffer, length):
    Post (TRUE, fclose(NetTrace)) \/ (TRUE, fclose(NetTrace))@*/

/*@ printoption(direction, cmd, option):
    Post (TRUE, fclose(NetTrace))@*/

/*@ SetForExit():
    Post (TRUE, fclose(__stdoutp) · fclose(__stderrp))@*/

/*@ another(pargc, pargv, prompt):
    Post (TRUE, fclose(__stdoutp)) \/ (TRUE, fclose(__stdoutp))@*/

/*@ mabort(signo):
    Post (TRUE, fclose(__stdoutp))@*/

/*@ remglob(argv, doswitch):
    Post (TRUE, fclose(ftemp)) \/ (!(ftemp=0), close(fd)) \/ ((!(ftemp=0)/\TRUE), close(fd)) \/ ((!(ftemp=0)/\TRUE), close(fd) · fclose(ftemp)) \/ ((!(ftemp=0)/\TRUE), close(fd)) \/ (((ftemp=0)/\TRUE), close(fd)) \/ (TRUE, fclose(ftemp))@*/

/*@ shell(argc, argv):
    Post (TRUE, close(pid)) \/ (TRUE, close(pid) · fclose(__stdoutp))@*/

/*@ user(argc, argv):
    Post (TRUE, fclose(__stdoutp)) \/ (TRUE, fclose(__stdoutp))@*/

/*@ disconnect(argc, argv):
    Post (TRUE, fclose(cout))@*/

/*@ confirm(cmd, file):
    Post (TRUE, fclose(__stdoutp)) \/ (TRUE, fclose(__stdoutp))@*/

/*@ doproxy(argc, argv):
    Post (TRUE, fclose(__stdoutp)) \/ (TRUE, fclose(__stdoutp)) \/ (TRUE, fclose(__stdoutp)) \/ (TRUE, fclose(__stdoutp))@*/

/*@ domacro(argc, argv):
    Post (TRUE, fclose(__stdoutp)) \/ (TRUE, fclose(__stdoutp)) \/ (TRUE, fclose(__stdoutp)) \/ (TRUE, fclose(__stdoutp)) \/ (TRUE, fclose(__stdoutp))@*/

/*@ login(host):
    Post (TRUE, fclose(__stdoutp))@*/

/*@ cmdabort(sig):
    Post (TRUE, fclose(__stdoutp))@*/

/*@ command(fmt):
    Post (TRUE, fclose(cout)) \/ (TRUE, fclose(__stdoutp) · fclose(cout)) \/ (TRUE, fclose(__stdoutp))@*/

/*@ getreply(expecteof):
    Post (TRUE, fclose(cout)) \/ (TRUE, fclose(cout) · fclose(__stdoutp)) \/ (TRUE, fclose(cout)) \/ (TRUE, fclose(cout) · fclose(__stdoutp)) \/ (TRUE, fclose(cout)) \/ (TRUE, fclose(cout)) \/ (TRUE, fclose(cout) · fclose(__stdoutp)) \/ (TRUE, fclose(cout)) \/ (TRUE, fclose(cout) · fclose(__stdoutp)) \/ (TRUE, fclose(cout)) \/ (TRUE, fclose(__stdoutp)) \/ (TRUE, fclose(__stdoutp))@*/

/*@ abortsend(sig):
    Post (TRUE, fclose(__stdoutp))@*/

/*@ abortrecv(sig):
    Post (TRUE, fclose(__stdoutp))@*/

/*@ dataconn(lmode):
    Post (TRUE, close(data)) \/ (TRUE, close(data))@*/

/*@ abortpt(sig):
    Post (TRUE, fclose(__stdoutp))@*/

/*@ abort_remote(din):
    Post (TRUE, fclose(cout))@*/

/*@ lostpeer(sig):
    Post (TRUE, fclose(cout)) \/ (TRUE, close(data)) \/ (TRUE, close(data) · fclose(cout)) \/ (TRUE, fclose(cout)) \/ (TRUE, fclose(cout) · fclose(cout)) \/ (TRUE, fclose(cout) · close(data)) \/ (TRUE, fclose(cout) · close(data) · fclose(cout))@*/

/*@ cmdscanner(top):
    Post (TRUE, fclose(__stdoutp))@*/

/*@ get_addrs(my_machine_name, his_machine_name):
    Post ((!((f<0))/\TRUE), close(f)) \/ (!((f<0)), close(f)) \/ ((!((f<0))/\(!((f<0))/\TRUE)), close(f) · close(f)) \/ ((!((f<0))/\!((f<0))), close(f) · close(f)) \/ ((!((f<0))/\!((f<0))), close(f) · close(f)) \/ (!((f<0)), close(f)) \/ (!((f<0)), close(f)) \/ ((!((f<0))/\TRUE), close(f) · close(f)) \/ (!((f<0)), close(f) · close(f)) \/ ((!((f<0))/\(!((f<0))/\TRUE)), close(f) · close(f) · close(f)) \/ ((!((f<0))/\!((f<0))), close(f) · close(f) · close(f)) \/ ((!((f<0))/\!((f<0))), close(f) · close(f) · close(f)) \/ (!((f<0)), close(f) · close(f)) \/ (!((f<0)), close(f) · close(f)) \/ (((f<0)/\TRUE), close(f)) \/ ((f<0), close(f)) \/ (((f<0)/\((f<0)/\TRUE)), close(f) · close(f)) \/ (((f<0)/\(f<0)), close(f) · close(f)) \/ (((f<0)/\(f<0)), close(f) · close(f)) \/ ((f<0), close(f)) \/ ((f<0), close(f)) \/ (((f<0)/\TRUE), close(f) · close(f)) \/ ((f<0), close(f) · close(f)) \/ (((f<0)/\((f<0)/\TRUE)), close(f) · close(f) · close(f)) \/ (((f<0)/\(f<0)), close(f) · close(f) · close(f)) \/ (((f<0)/\(f<0)), close(f) · close(f) · close(f)) \/ ((f<0), close(f) · close(f)) \/ ((f<0), close(f) · close(f))@*/

/*@ get_names(argc, argv):
    Post (((!((f<0))/\!((f<0)))/\TRUE), close(f) · close(f)) \/ ((!((f<0))/\TRUE), close(f)) \/ (((!((f<0))/\!((f<0)))/\TRUE), close(f) · close(f) · close(f)) \/ ((!((f<0))/\TRUE), close(f) · close(f)) \/ ((((f<0)/\(f<0))/\TRUE), close(f) · close(f)) \/ (((f<0)/\TRUE), close(f)) \/ ((((f<0)/\(f<0))/\TRUE), close(f) · close(f) · close(f)) \/ (((f<0)/\TRUE), close(f) · close(f))@*/

/*@ invite_remote():
    Post (TRUE, close(sockt))@*/

/*@ check_local():
    Post (TRUE, close(sockt))@*/

/*@ main(argc, argv):
    Post ((!((f<0))/\!((f<0))), close(f) · close(f) · close(sockt)) \/ ((!((f<0))/\!((f<0))), close(f) · close(f) · close(sockt) · close(sockt)) \/ (!((f<0)), close(f) · close(sockt)) \/ (!((f<0)), close(f) · close(sockt) · close(sockt)) \/ ((!((f<0))/\!((f<0))), close(f) · close(f) · close(f) · close(sockt)) \/ ((!((f<0))/\!((f<0))), close(f) · close(f) · close(f) · close(sockt) · close(sockt)) \/ (!((f<0)), close(f) · close(f) · close(sockt)) \/ (!((f<0)), close(f) · close(f) · close(sockt) · close(sockt)) \/ (((f<0)/\(f<0)), close(f) · close(f) · close(sockt)) \/ (((f<0)/\(f<0)), close(f) · close(f) · close(sockt) · close(sockt)) \/ ((f<0), close(f) · close(sockt)) \/ ((f<0), close(f) · close(sockt) · close(sockt)) \/ (((f<0)/\(f<0)), close(f) · close(f) · close(f) · close(sockt)) \/ (((f<0)/\(f<0)), close(f) · close(f) · close(f) · close(sockt) · close(sockt)) \/ ((f<0), close(f) · close(f) · close(sockt)) \/ ((f<0), close(f) · close(f) · close(sockt) · close(sockt))@*/

/*@ ping_finish():
    Post (TRUE, fclose(__stdoutp))@*/

/*@ echo_finish():
    Post (TRUE, fclose(__stdoutp))@*/

/*@ timestamp_finish():
    Post (TRUE, fclose(__stdoutp))@*/

/*@ ping_init(type, ident):
    Post ((!((fd<0))/\TRUE), CONSUME(fd)) \/ ((!((fd<0))/\TRUE), close(fd)) \/ (((fd<0)/\TRUE), 𝝐)@*/

/*@ ping_finish():
    Post (TRUE, fclose(__stdoutp))@*/

/*@ echo_finish():
    Post (TRUE, fclose(__stdoutp))@*/

/*@ ping_init(type, ident):
    Post ((!((fd<0))/\TRUE), CONSUME(fd)) \/ ((!((fd<0))/\TRUE), close(fd)) \/ ((!((fd<0))/\TRUE), close(fd)) \/ ((!((fd<0))/\TRUE), close(fd)) \/ (((fd<0)/\TRUE), 𝝐)@*/

/*@ main(argc, argv):
    Post ((!((sfd<0))/\TRUE), close(sfd)) \/ ((sfd<0), 𝝐)@*/

/*@ parse_opt_set_default_format_from_file(file):
    Post (!(fp=0), StmtExpr() · fclose(fp) · StmtExpr() · StmtExpr()) \/ ((fp=0), StmtExpr() · fclose(fp) · StmtExpr() · StmtExpr())@*/

