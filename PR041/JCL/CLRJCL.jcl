//RUNCOB JOB ,
// MSGCLASS=H,MSGLEVEL=(1,1),TIME=(,4),REGION=144M,COND=(16,LT)
//* //STP0001 EXEC PGM=IGYCRCTL
//* //SYSIN DD DISP=SHR,DSN=&SYSUID..COBOL(HELLOW)
//* //SYSLIN DD DISP=SHR,DSN=&SYSUID..OBJLIB(HELLOW)
//* //SYSPRINT DD SYSOUT=*
//* //STP0002 EXEC PGM=IEWL
//* //SYSLIN DD DISP=SHR,DSN=&SYSUID..OBJLIB(HELLOW)
//* //SYSLMOD DD DISP=SHR,DSN=&SYSUID..LOAD(HELLOW)
//* //SYSPRINT DD SYSOUT=*
//* //STP0003 EXEC PGM=IGZCRTN
//* //STEPLIB DD DISP=SHR,DSN=&SYSUID..LOAD(HELLOW)
//* //SYSPRINT DD SYSOUT=*
//* //SYSOUT DD SYSOUT=*
//*
//* //COMPLINK JOB
//* COMPILE AND LINK CALLEDPRG
//STEP1    EXEC IGYWCL,SRC='PR041'
//COBOL.SYSIN  DD DISP=SHR,DSN=Z41851.COBOL(PR041)
//STEP2 EXEC PGM=PR041
//CHNGE DD DSN=&SYSUID..DATA(CHFILE),DISP=SHR
//MSFILE DD DSN=&SYSUID..DATA(MSFILE),DISP=SHR
//OUTFILE DD DSN=&SYSUID..DATA.OUTFILE,
//    DISP=(NEW,CATLG,CATLG),
//    SPACE=(CYL,(10,5)),
//    DCB=(RECFM=FB,LRECL=80)
//RPT  DD DSN=&SYSUID..DATA.RPTFILE,
//    DISP=(NEW,CATLG,CATLG),
//    SPACE=(CYL,(10,5)),
//    DCB=(RECFM=FB,LRECL=80)
//STEPLIB DD DISP=SHR,DSN=&SYSUID..LOAD
//SYSPRINT DD SYSOUT=*
//SYSOUT DD SYSOUT=*

