#define SW_CHANNEL_MIN_MEM (1024*64)


/*@ Unified2InitFile(path):
    Future ((path>0), (!close(path))^* · close(path) · (_)^*)@*/

/*@ new_iterator(path):
    Future ((ret>0), (!close(ret))^* · close(ret) · (_)^*)@*/


/*@ SIP_AddUserDefinedMethod(path):
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^*) \/ ((ret=0), (!_(ret))^*)@*/

/*@ AppIdAddGenericConfigItem(path):
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^*)@*/

/*@ _sub_table_new(path):
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^*) \/ ((ret=0), (!_(ret))^*)@*/


/*@ parse_client_initiation(path):
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^*)@*/

/*@ updateMplsHeaders(path):
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^*)@*/

/*@ ServiceAddPort(path):
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^*)@*/


/*@ ClientAppRegisterPattern(path):
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^*)@*/

/*@ sipUaPatternAdd(path):
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^*)@*/


/*@ sipServerPatternAdd(path):
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^*)@*/


/*@ socket(domain, type, protocol):
    Future ((ret>0), (!close(ret))^* · close(ret) · (_)^*)@*/

/*@ close(handler):
    Post (TRUE, close(handler))@*/

/*@ fopen(path):
    Future (((ret>0)/\(!(ret=stdout)/\!(ret=stdin))), (!fclose(ret))^* · fclose(ret) · (_)^*)@*/


/*@ fclose(handler):
    Post (TRUE, fclose(handler))@*/

/*@ sfPolicyUserPolicySet(a, b):
    Post (TRUE, deref(a) · deref(b))@*/

/*@ fflush(handler):
    Post (TRUE, fclose(handler))@*/

/*@ opendir(path):
    Future ((ret>0), (!closedir(ret))^* · closedir(ret) · (_)^*)@*/

/*@ closedir(handler):
    Post (TRUE, closedir(handler))@*/

/*@ sflist_init(handler):
    Post (TRUE, deref(handler))@*/

/*@ getSessionPlugins():
    Future ((ret=0), (!_(ret))^*)@*/

/*@ sfPolicyUserDataGet(a, b):
    Future ((ret=0), (!_(ret))^*)@*/
    
/*@ sfPolicyUserDataGetDefault(frag3_swap_config):
    Future ((ret=0), (!_(ret))^*)@*/

/*@ sfPolicyUserDataGetCurrent(frag3_config):
    Future ((ret=0), (!_(ret))^*)@*/

/*@ sfPolicyConfigCreate():
    Future ((ret=0), (!_(ret))^*)@*/

/*@ sflist_new():
    Future ((ret=0), (!_(ret))^*)@*/

/*@ localtime(t):
    Future ((ret=0), (!_(ret))^*)@*/

/*@ malloc(path):
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^*) \/ ((ret=0), (!_(ret))^*)@*/

/*@ getRuntimeRtnFromOtn(path):
    Future ((ret=0), (!_(ret))^*)@*/

/*@ AC_MALLOC(path):
    Future ((ret=0), (!_(ret))^*)@*/

/* calloc(path):
    Future ((ret=0), (!_(ret))^*)@*/

/*@ xmalloc(path):
    Future (!(ret=0), (!free(ret))^* · free(ret) · (_)^*) @*/

/*@ realloc(a, b):
    Future ((ret=0), (!_(ret))^*)@*/

/*@ free(handler):
    Post (TRUE, free(handler))@*/

/*@ free_iterator(handler): 
    Post (TRUE, free(handler) · close(handler))   @*/


/*@ strcmp(a, b):
    Post (TRUE, strcmp(a))@*/

/*@ fputs(a, b):
    Post (TRUE, fputs(b))@*/

/*@ memset(a, b):
    Post (TRUE, memset(a))@*/

/*@ memcpy(a, b):
    Post (TRUE, memcpy(a))@*/

/*@ strcpy(a, b): 
    Post (TRUE, strcpy(a))  @*/

/*@ strlen(a):
    Post (TRUE, strlen(a))@*/

/*@ sprintf(a, b, c):
    Post (TRUE, sprintf(a))@*/

/*@ sprintf(a, b, c):
    Post (TRUE, sprintf(a))@*/

/*@ processHTTPPacket(p, session, direction, headers, pConfig):
    Post (TRUE, CONSUME(session))@*/















/*@ asn1_decode_type(t):
    Future (TRUE, (_)^*)@*/

/*@ asn1_decode_type(t):
    Future (TRUE, (_)^*)@*/

/*@ asn1_node_alloc():
    Future (TRUE, (_)^*)@*/

/*@ sfghash_findnext(t):
    Future (TRUE, (_)^*)@*/

/*@ sfxhash_find(t, key):
    Future (TRUE, (_)^*)@*/

/*@ sfxhash_findfirst(t):
    Future (TRUE, (_)^*)@*/

/*@ getFlowBitItem(flowbitName, flowbits, flowbits_grp):
    Future (TRUE, (_)^*)@*/

/*@ sfghash_find(t, key):
    Future (TRUE, (_)^*)@*/


/*@ NewNode(otn, type):
    Future (TRUE, (_)^*)@*/


/*@ sfghash_findfirst(t):
    Future (TRUE, (_)^*)@*/

/*@ getFlowBitGroup(groupName):
    Future (TRUE, (_)^*)@*/

/*@ sfrt_new(table_type, ip_type, data_size, mem_cap):
    Future (TRUE, (_)^*)@*/

/*@ SegmentAlloc(p, tv, caplen, pktlen, pkt):
    Future (TRUE, (_)^*)@*/


/*@ http_cmd_lookup_find(CmdLookup, cmd, len, iError):
    Future (TRUE, (_)^*)@*/

/*@ sfxhash_mru(t):
    Future (TRUE, (_)^*)@*/


/*@ ftp_cmd_lookup_first(CmdLookup, iError):
    Future (TRUE, (_)^*)@*/

/*@ GetDynamicFastPatternPmd(dd, dd_type):
    Future (TRUE, (_)^*)@*/

/*@ GetLongestPmdContent(otn, type):
    Future (TRUE, (_)^*)@*/

/*@ DynamicFpContentsToPmdList(fp_list):
    Future (TRUE, (_)^*)@*/

/*@ sfvar_create_alias(alias_from, alias_to):
    Future (TRUE, (_)^*)@*/

/*@ get_create_file_session(ssnptr):
    Future (TRUE, (_)^*)@*/

/*@ sfrt_lookup(ip, table):
    Future (TRUE, (_)^*)@*/

/*@ sfxhash_gfindfirst(t):
    Future (TRUE, (_)^*)@*/

/*@ fwAvlSerialize(tree):
    Future (TRUE, (_)^*)@*/



/*@ AppIdFindGenericConfigItem(pConfig, name):
    Future (TRUE, (_)^*)@*/


/*@ ftp_cmd_lookup_find(CmdLookup, cmd, len, iError):
    Future (TRUE, (_)^*)@*/

/*@ DCE2_ListNext(list):
    Future (TRUE, (_)^*)@*/

/*@ DCE2_Alloc(size, mtype):
    Future (TRUE, (_)^*)@*/

/*@ DCE2_BufferNew(initial_size, min_add_size, mem_type):
    Future (TRUE, (_)^*)@*/


/*@ DCE2_ListFirst(list):
    Future (TRUE, (_)^*)@*/

/*@ DCE2_QueueNext(queue):
    Future (TRUE, (_)^*)@*/

/*@ DCE2_ListFind(list, key):
    Future (TRUE, (_)^*)@*/

/*@ NewSDFSession(config, packet):
    Future (TRUE, (_)^*)@*/

/*@ sfrt_flat_new(table_flat_type, ip_type, data_size, mem_cap):
    Future (TRUE, (_)^*)@*/


/*@ initStreamPolicyConfig(sc, reload_config):
    Future (TRUE, (_)^*)@*/
