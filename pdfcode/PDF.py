#! /usr/bin/env python

# Python LHAPDF6 usage example

import LHAPDF
import ROOT
import math
from array import array

#samples = {"run_03": 0.000318645, "run_04": 0.00041858, "run_05": 0.000417874, "run_06": 0.000341204, "run_07": 0.00040066464, "run_08": 7.46949e-06, "run_09": 8.164937e-05, "run_10": 0.000339939, "run_11": 0.000340502, "run_12": 0.00035562703, "run_13": 0.0002492081}
samples = {"run_10": 0.000339939}


files = {"P1_Sigma_HC_UFO_ccx_epemmupmumddx":[4 , -4], "P1_Sigma_HC_UFO_ccx_epemmupmumdsx":[4 , -4], "P1_Sigma_HC_UFO_ccx_epemmupmumsdx":[4 , -4], "P1_Sigma_HC_UFO_ccx_epemmupmumssx":[4 , -4], "P1_Sigma_HC_UFO_cd_epemmupmumcd":[4, 1], "P1_Sigma_HC_UFO_cd_epemmupmumcs":[4, 1], "P1_Sigma_HC_UFO_cd_epemmupmumud":[4, 1], "P1_Sigma_HC_UFO_cd_epemmupmumus":[4, 1], "P1_Sigma_HC_UFO_cs_epemmupmumcd":[4 , 3], "P1_Sigma_HC_UFO_cs_epemmupmumcs":[4 , 3], "P1_Sigma_HC_UFO_cs_epemmupmumud":[4 , 3], "P1_Sigma_HC_UFO_cs_epemmupmumus":[4 , 3], "P1_Sigma_HC_UFO_cux_epemmupmumddx":[4, -2], "P1_Sigma_HC_UFO_cux_epemmupmumdsx":[4, -2], "P1_Sigma_HC_UFO_cux_epemmupmumsdx":[4, -2], "P1_Sigma_HC_UFO_cux_epemmupmumssx":[4, -2], "P1_Sigma_HC_UFO_cxdx_epemmupmumcxdx":[-4, -1], "P1_Sigma_HC_UFO_cxdx_epemmupmumcxsx":[-4, -1], "P1_Sigma_HC_UFO_cxdx_epemmupmumuxdx":[-4, -1], "P1_Sigma_HC_UFO_cxdx_epemmupmumuxsx":[-4, -1], "P1_Sigma_HC_UFO_cxsx_epemmupmumcxdx":[-4, -3], "P1_Sigma_HC_UFO_cxsx_epemmupmumcxsx":[-4, -3], "P1_Sigma_HC_UFO_cxsx_epemmupmumuxdx":[-4, -3], "P1_Sigma_HC_UFO_cxsx_epemmupmumuxsx":[-4, -3], "P1_Sigma_HC_UFO_dd_epemmupmumdd":[1, 1], "P1_Sigma_HC_UFO_ddx_epemmupmumccx":[1, -1], "P1_Sigma_HC_UFO_ddx_epemmupmumcux":[1, -1], "P1_Sigma_HC_UFO_ddx_epemmupmumddx":[1, -1], "P1_Sigma_HC_UFO_ddx_epemmupmumucx":[1, -1], "P1_Sigma_HC_UFO_ddx_epemmupmumuux":[1, -1], "P1_Sigma_HC_UFO_ds_epemmupmumds":[1, 3], "P1_Sigma_HC_UFO_dsx_epemmupmumccx":[1, -3], "P1_Sigma_HC_UFO_dsx_epemmupmumcux":[1, -3], "P1_Sigma_HC_UFO_dsx_epemmupmumucx":[1, -3], "P1_Sigma_HC_UFO_dsx_epemmupmumuux":[1, -3], "P1_Sigma_HC_UFO_dux_epemmupmumdux":[1, -2], "P1_Sigma_HC_UFO_dxdx_epemmupmumdxdx":[-1, -1], "P1_Sigma_HC_UFO_dxsx_epemmupmumdxsx":[-1, -3], "P1_Sigma_HC_UFO_gg_epemmupmumgg":[21, 21], "P1_Sigma_HC_UFO_sdx_epemmupmumccx":[3, -1], "P1_Sigma_HC_UFO_sdx_epemmupmumcux":[3, -1], "P1_Sigma_HC_UFO_sdx_epemmupmumucx":[3, -1], "P1_Sigma_HC_UFO_sdx_epemmupmumuux":[3, -1], "P1_Sigma_HC_UFO_ssx_epemmupmumccx":[3, -3], "P1_Sigma_HC_UFO_ssx_epemmupmumcux":[3, -3], "P1_Sigma_HC_UFO_ssx_epemmupmumucx":[3, -3], "P1_Sigma_HC_UFO_ssx_epemmupmumuux":[3, -3], "P1_Sigma_HC_UFO_uc_epemmupmumuc":[2, 4], "P1_Sigma_HC_UFO_ucx_epemmupmumddx":[2, -4], "P1_Sigma_HC_UFO_ucx_epemmupmumdsx":[2, -4], "P1_Sigma_HC_UFO_ucx_epemmupmumsdx":[2, -4], "P1_Sigma_HC_UFO_ucx_epemmupmumssx":[2, -4], "P1_Sigma_HC_UFO_ud_epemmupmumcd":[2, 1], "P1_Sigma_HC_UFO_ud_epemmupmumcs":[2, 1], "P1_Sigma_HC_UFO_ud_epemmupmumud":[2, 1], "P1_Sigma_HC_UFO_ud_epemmupmumus":[2, 1], "P1_Sigma_HC_UFO_udx_epemmupmumudx":[2, -1], "P1_Sigma_HC_UFO_us_epemmupmumcd":[2, 3], "P1_Sigma_HC_UFO_us_epemmupmumcs":[2, 3], "P1_Sigma_HC_UFO_us_epemmupmumud":[2, 3], "P1_Sigma_HC_UFO_us_epemmupmumus":[2, 3], "P1_Sigma_HC_UFO_uu_epemmupmumuu":[2, 2], "P1_Sigma_HC_UFO_uux_epemmupmumddx":[2, -2], "P1_Sigma_HC_UFO_uux_epemmupmumdsx":[2, -2], "P1_Sigma_HC_UFO_uux_epemmupmumsdx":[2, -2], "P1_Sigma_HC_UFO_uux_epemmupmumssx":[2, -2], "P1_Sigma_HC_UFO_uux_epemmupmumuux":[2, -2], "P1_Sigma_HC_UFO_uxcx_epemmupmumuxcx":[-2, -4], "P1_Sigma_HC_UFO_uxdx_epemmupmumcxdx":[-2, -1], "P1_Sigma_HC_UFO_uxdx_epemmupmumcxsx":[-2, -1], "P1_Sigma_HC_UFO_uxdx_epemmupmumuxdx":[-2, -1], "P1_Sigma_HC_UFO_uxdx_epemmupmumuxsx":[-2, -1], "P1_Sigma_HC_UFO_uxsx_epemmupmumcxdx":[-2, -3], "P1_Sigma_HC_UFO_uxsx_epemmupmumcxsx":[-2, -3], "P1_Sigma_HC_UFO_uxsx_epemmupmumuxdx":[-2, -3], "P1_Sigma_HC_UFO_uxsx_epemmupmumuxsx":[-2, -3], "P1_Sigma_HC_UFO_uxux_epemmupmumuxux":[-2, -2]}
# Getting a PDF member object
#p = lhapdf.mkPDF("CT10", 0)
p = lhapdf.mkPDF("NNPDF23_lo_as_0130_qed/0")
nStep = 0

for sample in samples:
    nStep +=1
    counterFile = ROOT.TFile("output/ME/" + sample + "/Counter.root", "open")
    counterTree = counterFile.Get("t_MESM")
    nEvents = counterTree.GetEntries()
    counterFile.Close()
    
    outputFile  = ROOT.TFile("output/OutputVBFME_" + sample + ".root", "recreate")
    Events = ROOT.TTree("mini4l", "mini4l")
    #branches = {
    #'O1': 'F',
    #'O2': 'F',
    #'XS': 'F',}
    #Events.create_branches(branches)
    #O1 = ROOT.TTree("O1", "O1")
    #O2 = ROOT.TTree("O2", "O2")
    #XS = ROOT.TTree("XS", "XS")
    value1 = array('f', [0.]) #Creating a branch variable
    value2 = array('f', [0.]) #Creating a branch variable
    value3 = array('f', [0.]) #Creating a branch variable
    value4 = array('f', [0.]) #Creating a branch variable
    Events.Branch("OO1", value1, "OO1" + "/F")
    Events.Branch("OO2", value2, "OO2" + "/F")
    Events.Branch("w_xs_br", value3, "w_xs_br" + "/F")
    Events.Branch("w_MCw", value4, "w_MCw" + "/F")
    
    inputFile = []
    for name in files:
        file = ROOT.TFile("output/ME/" + sample + "/" + name + ".root", "open")
        inputFile.append(file)
        #print str(nEvents)
    for i in range(nEvents):
        if i%5000 == 0: print( str(i) + " events have been processed")
        SM = 0
        BSM = 0
        MIX = 0 
        for j in range(len(inputFile)):
            #inputFile = ROOT.TFile("output/ME/" + name + ".root", "open")
            inputSMTree = inputFile[j].Get("t_MESM")
            inputBSMTree = inputFile[j].Get("t_MEBSM")
            inputMIXTree = inputFile[j].Get("t_MEMIX")
            inputX1Tree = inputFile[j].Get("t_X1")
            inputX2Tree = inputFile[j].Get("t_X2")
            inputX1Tree.GetEntry(i)
            X1 = inputX1Tree.tX1
            inputX2Tree.GetEntry(i)
            X2 = inputX2Tree.tX2
            #CHANGE THE VALUES OF NAME
            name = str(inputFile[j])[38:-21]
            #print "===============================================" + str(name) + "==============================================="
            #print name
            c = p.xfxQ(files[name][0], X1, 1.25e2)*p.xfxQ(files[name][1], X2, 1.25e2)
            #print "X1 = " + str(X1) + "; X2 = " + str(X2) + ";"
            inputSMTree.GetEntry(i)
            SM = SM + c*inputSMTree.MESM
            #print "Weighted_ME_SM = " + str(SM) + ";"
            inputBSMTree.GetEntry(i)
            BSM = BSM + c*inputBSMTree.MEBSM
            #print "Weighted_ME_BSM = " + str(BSM) + ";"
            inputMIXTree.GetEntry(i)
            MIX = MIX + c*inputMIXTree.MEMIX
            #print "ME_SM = " + str(inputSMTree.MESM) + "; ME_BSM = " + str(inputBSMTree.MEBSM) + "; ME_MIX = " + str(inputMIXTree.MEMIX)
            #print "Weight1 = " + str(p.xfxQ(files[name][0], X1, 1.25e2)) + "; Weight2 = " + str(p.xfxQ(files[name][1], X2, 1.25e2)) + ";"
            #print "Weighted_ME_SM = " + str(c*inputSMTree.MESM) + "; Weighted_ME_BSM = " + str(c*inputBSMTree.MEBSM) + "; Weighted_ME_MIX = " + str(c*inputMIXTree.MEMIX)
            #print "Weighted_ME_MIX = " + str(MIX) + ";"
            #print "===============================================" + str(name) + "==============================================="
            #print ""
        value1[0] = (MIX - BSM - SM - 2*math.sqrt(BSM)*math.sqrt(SM))/SM 
        value2[0] = BSM/SM
        value3[0] = samples[sample]
        value4[0] = samples[sample] / nEvents
        Events.Fill()
        #print "Final_ME_SM = " + str(SM) + "; Final_ME_BSM = " + str(BSM) + "; Final_ME_MIX = " + str(MIX)
    #OO.Fill()
    #O1.Fill()
    #O2.Fill()
    #    print str(totO1)
    #    print str(totO2)
    #    print "Processing event " + str(i) + "..."
    #XS.Fill()
    
    
    #Events.Fill()
    outputFile.Write()
    outputFile.Close()

# PDF querying at different x with Q=125 GeV
#print p.xfxQ(21, 1e-3, 1.25e2)
#print p.xfxQ(3, 1e-3, 1.25e2)

## Basic all-flavour PDF querying at x=0.01, Q=M_Z
#for pid in p.flavors():
#    print p.xfxQ(pid, 0.01, 91.2)
#
## TODO: demonstrate looping over PDF set members
#pset = lhapdf.getPDFSet("CT10nlo")
#print pset.description
#pcentral = pset.mkPDF(0)
#pdfs1 = pset.mkPDFs()
#pdfs2 = lhapdf.mkPDFs("CT10nlo") # a direct way to get all the set's PDFs
#
## Filling a numpy 2D array with xf(x,Q) samples
#import numpy as np
#xs = [x for x in np.logspace(-7, 0, 5)]
#qs = [q for q in np.logspace(1, 4, 4)]
#gluon_xfs = np.empty([len(xs), len(qs)])
#for ix, x in enumerate(xs):
#    for iq, q in enumerate(qs):
#        gluon_xfs[ix,iq] = p.xfxQ(21, x, q)
#print gluon_xfs
#
## Version info, search paths, and metadata
#print lhapdf.version()
#print lhapdf.__version__
#lhapdf.pathsPrepend("/path/to/extra/pdfsets")
#print lhapdf.paths()
## ...
