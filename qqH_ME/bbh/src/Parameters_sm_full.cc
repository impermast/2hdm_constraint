//==========================================================================
// This file has been automatically generated for C++ by
// MadGraph5_aMC@NLO v. 3.1.1, 2021-05-28
// By the MadGraph5_aMC@NLO Development Team
// Visit launchpad.net/madgraph5 and amcatnlo.web.cern.ch
//==========================================================================

#include <iostream> 
#include <iomanip> 
#include "Parameters_sm_full.h"

// Initialize static instance
Parameters_sm_full * Parameters_sm_full::instance = 0; 

// Function to get static instance - only one instance per program
Parameters_sm_full * Parameters_sm_full::getInstance()
{
  if (instance == 0)
    instance = new Parameters_sm_full(); 

  return instance; 
}

void Parameters_sm_full::setIndependentParameters(SLHAReader& slha)
{
  // Define "zero"
  zero = 0; 
  ZERO = 0; 
  // Prepare a vector for indices
  vector<int> indices(2, 0); 
  mdl_WH = slha.get_block_entry("decay", 25, 6.382339e-03); 
  mdl_WW = slha.get_block_entry("decay", 24, 2.047600e+00); 
  mdl_WZ = slha.get_block_entry("decay", 23, 2.441404e+00); 
  mdl_WTau = slha.get_block_entry("decay", 15, 2.270000e-12); 
  mdl_WT = slha.get_block_entry("decay", 6, 1.508336e+00); 
  mdl_ymtau = slha.get_block_entry("yukawa", 15, 1.777000e+00); 
  mdl_ymm = slha.get_block_entry("yukawa", 13, 1.056600e-01); 
  mdl_yme = slha.get_block_entry("yukawa", 11, 5.110000e-04); 
  mdl_ymt = slha.get_block_entry("yukawa", 6, 1.645000e+02); 
  mdl_ymb = slha.get_block_entry("yukawa", 5, 4.200000e+00); 
  mdl_ymc = slha.get_block_entry("yukawa", 4, 1.270000e+00); 
  mdl_etaWS = slha.get_block_entry("wolfenstein", 4, 3.410000e-01); 
  mdl_rhoWS = slha.get_block_entry("wolfenstein", 3, 1.320000e-01); 
  mdl_AWS = slha.get_block_entry("wolfenstein", 2, 8.080000e-01); 
  mdl_lamWS = slha.get_block_entry("wolfenstein", 1, 2.253000e-01); 
  aS = slha.get_block_entry("sminputs", 3, 1.180000e-01); 
  mdl_Gf = slha.get_block_entry("sminputs", 2, 1.166390e-05); 
  aEWM1 = slha.get_block_entry("sminputs", 1, 1.325070e+02); 
  mdl_MH = slha.get_block_entry("mass", 25, 1.250000e+02); 
  mdl_MZ = slha.get_block_entry("mass", 23, 9.118800e+01); 
  mdl_MTA = slha.get_block_entry("mass", 15, 1.777000e+00); 
  mdl_MM = slha.get_block_entry("mass", 13, 1.056600e-01); 
  mdl_Me = slha.get_block_entry("mass", 11, 5.110000e-04); 
  mdl_MT = slha.get_block_entry("mass", 6, 1.720000e+02); 
  mdl_MB = slha.get_block_entry("mass", 5, 4.700000e+00); 
  mdl_MC = slha.get_block_entry("mass", 4, 1.270000e+00); 
  mdl_lamWS__exp__2 = ((mdl_lamWS) * (mdl_lamWS)); 
  mdl_CKM1x1 = 1. - mdl_lamWS__exp__2/2.; 
  mdl_CKM1x2 = mdl_lamWS; 
  mdl_complexi = std::complex<double> (0., 1.); 
  mdl_lamWS__exp__3 = ((mdl_lamWS) * (mdl_lamWS) * (mdl_lamWS)); 
  mdl_CKM1x3 = mdl_AWS * mdl_lamWS__exp__3 * (-(mdl_etaWS * mdl_complexi) +
      mdl_rhoWS);
  mdl_CKM2x1 = -mdl_lamWS; 
  mdl_CKM2x2 = 1. - mdl_lamWS__exp__2/2.; 
  mdl_CKM2x3 = mdl_AWS * mdl_lamWS__exp__2; 
  mdl_CKM3x1 = mdl_AWS * mdl_lamWS__exp__3 * (1. - mdl_etaWS * mdl_complexi -
      mdl_rhoWS);
  mdl_CKM3x2 = -(mdl_AWS * mdl_lamWS__exp__2); 
  mdl_CKM3x3 = 1.; 
  mdl_MZ__exp__2 = ((mdl_MZ) * (mdl_MZ)); 
  mdl_MZ__exp__4 = ((mdl_MZ) * (mdl_MZ) * (mdl_MZ) * (mdl_MZ)); 
  mdl_sqrt__2 = sqrt(2.); 
  mdl_MH__exp__2 = ((mdl_MH) * (mdl_MH)); 
  mdl_conjg__CKM1x3 = conj(mdl_CKM1x3); 
  mdl_conjg__CKM2x3 = conj(mdl_CKM2x3); 
  mdl_conjg__CKM3x3 = conj(mdl_CKM3x3); 
  mdl_conjg__CKM2x1 = conj(mdl_CKM2x1); 
  mdl_conjg__CKM3x1 = conj(mdl_CKM3x1); 
  mdl_conjg__CKM2x2 = conj(mdl_CKM2x2); 
  mdl_conjg__CKM3x2 = conj(mdl_CKM3x2); 
  mdl_conjg__CKM1x1 = conj(mdl_CKM1x1); 
  mdl_conjg__CKM1x2 = conj(mdl_CKM1x2); 
  mdl_aEW = 1./aEWM1; 
  mdl_MW = sqrt(mdl_MZ__exp__2/2. + sqrt(mdl_MZ__exp__4/4. - (mdl_aEW * M_PI *
      mdl_MZ__exp__2)/(mdl_Gf * mdl_sqrt__2)));
  mdl_sqrt__aEW = sqrt(mdl_aEW); 
  mdl_ee = 2. * mdl_sqrt__aEW * sqrt(M_PI); 
  mdl_MW__exp__2 = ((mdl_MW) * (mdl_MW)); 
  mdl_sw2 = 1. - mdl_MW__exp__2/mdl_MZ__exp__2; 
  mdl_cw = sqrt(1. - mdl_sw2); 
  mdl_sqrt__sw2 = sqrt(mdl_sw2); 
  mdl_sw = mdl_sqrt__sw2; 
  mdl_g1 = mdl_ee/mdl_cw; 
  mdl_gw = mdl_ee/mdl_sw; 
  mdl_vev = (2. * mdl_MW * mdl_sw)/mdl_ee; 
  mdl_vev__exp__2 = ((mdl_vev) * (mdl_vev)); 
  mdl_lam = mdl_MH__exp__2/(2. * mdl_vev__exp__2); 
  mdl_yb = (mdl_ymb * mdl_sqrt__2)/mdl_vev; 
  mdl_yc = (mdl_ymc * mdl_sqrt__2)/mdl_vev; 
  mdl_ye = (mdl_yme * mdl_sqrt__2)/mdl_vev; 
  mdl_ym = (mdl_ymm * mdl_sqrt__2)/mdl_vev; 
  mdl_yt = (mdl_ymt * mdl_sqrt__2)/mdl_vev; 
  mdl_ytau = (mdl_ymtau * mdl_sqrt__2)/mdl_vev; 
  mdl_muH = sqrt(mdl_lam * mdl_vev__exp__2); 
  mdl_I1x31 = mdl_yb * mdl_conjg__CKM1x3; 
  mdl_I1x32 = mdl_yb * mdl_conjg__CKM2x3; 
  mdl_I1x33 = mdl_yb * mdl_conjg__CKM3x3; 
  mdl_I2x12 = mdl_yc * mdl_conjg__CKM2x1; 
  mdl_I2x13 = mdl_yt * mdl_conjg__CKM3x1; 
  mdl_I2x22 = mdl_yc * mdl_conjg__CKM2x2; 
  mdl_I2x23 = mdl_yt * mdl_conjg__CKM3x2; 
  mdl_I2x32 = mdl_yc * mdl_conjg__CKM2x3; 
  mdl_I2x33 = mdl_yt * mdl_conjg__CKM3x3; 
  mdl_I3x21 = mdl_CKM2x1 * mdl_yc; 
  mdl_I3x22 = mdl_CKM2x2 * mdl_yc; 
  mdl_I3x23 = mdl_CKM2x3 * mdl_yc; 
  mdl_I3x31 = mdl_CKM3x1 * mdl_yt; 
  mdl_I3x32 = mdl_CKM3x2 * mdl_yt; 
  mdl_I3x33 = mdl_CKM3x3 * mdl_yt; 
  mdl_I4x13 = mdl_CKM1x3 * mdl_yb; 
  mdl_I4x23 = mdl_CKM2x3 * mdl_yb; 
  mdl_I4x33 = mdl_CKM3x3 * mdl_yb; 
  mdl_ee__exp__2 = ((mdl_ee) * (mdl_ee)); 
  mdl_sw__exp__2 = ((mdl_sw) * (mdl_sw)); 
  mdl_cw__exp__2 = ((mdl_cw) * (mdl_cw)); 
}
void Parameters_sm_full::setIndependentCouplings()
{
  GC_83 = -((mdl_complexi * mdl_yb)/mdl_sqrt__2); 
}
void Parameters_sm_full::setDependentParameters()
{
  mdl_sqrt__aS = sqrt(aS); 
  G = 2. * mdl_sqrt__aS * sqrt(M_PI); 
  mdl_G__exp__2 = ((G) * (G)); 
}
void Parameters_sm_full::setDependentCouplings()
{

}

// Routines for printing out parameters
/*
void Parameters_sm_full::printIndependentParameters()
{
  cout <<  "sm_full model parameters independent of event kinematics:" << endl; 
  cout << setw(20) <<  "mdl_WH " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_WH << endl;
  cout << setw(20) <<  "mdl_WW " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_WW << endl;
  cout << setw(20) <<  "mdl_WZ " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_WZ << endl;
  cout << setw(20) <<  "mdl_WTau " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_WTau << endl;
  cout << setw(20) <<  "mdl_WT " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_WT << endl;
  cout << setw(20) <<  "mdl_ymtau " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_ymtau << endl;
  cout << setw(20) <<  "mdl_ymm " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_ymm << endl;
  cout << setw(20) <<  "mdl_yme " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_yme << endl;
  cout << setw(20) <<  "mdl_ymt " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_ymt << endl;
  cout << setw(20) <<  "mdl_ymb " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_ymb << endl;
  cout << setw(20) <<  "mdl_ymc " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_ymc << endl;
  cout << setw(20) <<  "mdl_etaWS " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_etaWS << endl;
  cout << setw(20) <<  "mdl_rhoWS " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_rhoWS << endl;
  cout << setw(20) <<  "mdl_AWS " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_AWS << endl;
  cout << setw(20) <<  "mdl_lamWS " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_lamWS << endl;
  cout << setw(20) <<  "aS " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << aS << endl;
  cout << setw(20) <<  "mdl_Gf " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_Gf << endl;
  cout << setw(20) <<  "aEWM1 " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << aEWM1 << endl;
  cout << setw(20) <<  "mdl_MH " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_MH << endl;
  cout << setw(20) <<  "mdl_MZ " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_MZ << endl;
  cout << setw(20) <<  "mdl_MTA " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_MTA << endl;
  cout << setw(20) <<  "mdl_MM " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_MM << endl;
  cout << setw(20) <<  "mdl_Me " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_Me << endl;
  cout << setw(20) <<  "mdl_MT " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_MT << endl;
  cout << setw(20) <<  "mdl_MB " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_MB << endl;
  cout << setw(20) <<  "mdl_MC " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_MC << endl;
  cout << setw(20) <<  "mdl_lamWS__exp__2 " <<  "= " <<
      setiosflags(ios::scientific) << setw(10) << mdl_lamWS__exp__2 << endl;
  cout << setw(20) <<  "mdl_CKM1x1 " <<  "= " << setiosflags(ios::scientific)
      << setw(10) << mdl_CKM1x1 << endl;
  cout << setw(20) <<  "mdl_CKM1x2 " <<  "= " << setiosflags(ios::scientific)
      << setw(10) << mdl_CKM1x2 << endl;
  cout << setw(20) <<  "mdl_complexi " <<  "= " << setiosflags(ios::scientific)
      << setw(10) << mdl_complexi << endl;
  cout << setw(20) <<  "mdl_lamWS__exp__3 " <<  "= " <<
      setiosflags(ios::scientific) << setw(10) << mdl_lamWS__exp__3 << endl;
  cout << setw(20) <<  "mdl_CKM1x3 " <<  "= " << setiosflags(ios::scientific)
      << setw(10) << mdl_CKM1x3 << endl;
  cout << setw(20) <<  "mdl_CKM2x1 " <<  "= " << setiosflags(ios::scientific)
      << setw(10) << mdl_CKM2x1 << endl;
  cout << setw(20) <<  "mdl_CKM2x2 " <<  "= " << setiosflags(ios::scientific)
      << setw(10) << mdl_CKM2x2 << endl;
  cout << setw(20) <<  "mdl_CKM2x3 " <<  "= " << setiosflags(ios::scientific)
      << setw(10) << mdl_CKM2x3 << endl;
  cout << setw(20) <<  "mdl_CKM3x1 " <<  "= " << setiosflags(ios::scientific)
      << setw(10) << mdl_CKM3x1 << endl;
  cout << setw(20) <<  "mdl_CKM3x2 " <<  "= " << setiosflags(ios::scientific)
      << setw(10) << mdl_CKM3x2 << endl;
  cout << setw(20) <<  "mdl_CKM3x3 " <<  "= " << setiosflags(ios::scientific)
      << setw(10) << mdl_CKM3x3 << endl;
  cout << setw(20) <<  "mdl_MZ__exp__2 " <<  "= " <<
      setiosflags(ios::scientific) << setw(10) << mdl_MZ__exp__2 << endl;
  cout << setw(20) <<  "mdl_MZ__exp__4 " <<  "= " <<
      setiosflags(ios::scientific) << setw(10) << mdl_MZ__exp__4 << endl;
  cout << setw(20) <<  "mdl_sqrt__2 " <<  "= " << setiosflags(ios::scientific)
      << setw(10) << mdl_sqrt__2 << endl;
  cout << setw(20) <<  "mdl_MH__exp__2 " <<  "= " <<
      setiosflags(ios::scientific) << setw(10) << mdl_MH__exp__2 << endl;
  cout << setw(20) <<  "mdl_conjg__CKM1x3 " <<  "= " <<
      setiosflags(ios::scientific) << setw(10) << mdl_conjg__CKM1x3 << endl;
  cout << setw(20) <<  "mdl_conjg__CKM2x3 " <<  "= " <<
      setiosflags(ios::scientific) << setw(10) << mdl_conjg__CKM2x3 << endl;
  cout << setw(20) <<  "mdl_conjg__CKM3x3 " <<  "= " <<
      setiosflags(ios::scientific) << setw(10) << mdl_conjg__CKM3x3 << endl;
  cout << setw(20) <<  "mdl_conjg__CKM2x1 " <<  "= " <<
      setiosflags(ios::scientific) << setw(10) << mdl_conjg__CKM2x1 << endl;
  cout << setw(20) <<  "mdl_conjg__CKM3x1 " <<  "= " <<
      setiosflags(ios::scientific) << setw(10) << mdl_conjg__CKM3x1 << endl;
  cout << setw(20) <<  "mdl_conjg__CKM2x2 " <<  "= " <<
      setiosflags(ios::scientific) << setw(10) << mdl_conjg__CKM2x2 << endl;
  cout << setw(20) <<  "mdl_conjg__CKM3x2 " <<  "= " <<
      setiosflags(ios::scientific) << setw(10) << mdl_conjg__CKM3x2 << endl;
  cout << setw(20) <<  "mdl_conjg__CKM1x1 " <<  "= " <<
      setiosflags(ios::scientific) << setw(10) << mdl_conjg__CKM1x1 << endl;
  cout << setw(20) <<  "mdl_conjg__CKM1x2 " <<  "= " <<
      setiosflags(ios::scientific) << setw(10) << mdl_conjg__CKM1x2 << endl;
  cout << setw(20) <<  "mdl_aEW " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_aEW << endl;
  cout << setw(20) <<  "mdl_MW " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_MW << endl;
  cout << setw(20) <<  "mdl_sqrt__aEW " <<  "= " <<
      setiosflags(ios::scientific) << setw(10) << mdl_sqrt__aEW << endl;
  cout << setw(20) <<  "mdl_ee " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_ee << endl;
  cout << setw(20) <<  "mdl_MW__exp__2 " <<  "= " <<
      setiosflags(ios::scientific) << setw(10) << mdl_MW__exp__2 << endl;
  cout << setw(20) <<  "mdl_sw2 " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_sw2 << endl;
  cout << setw(20) <<  "mdl_cw " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_cw << endl;
  cout << setw(20) <<  "mdl_sqrt__sw2 " <<  "= " <<
      setiosflags(ios::scientific) << setw(10) << mdl_sqrt__sw2 << endl;
  cout << setw(20) <<  "mdl_sw " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_sw << endl;
  cout << setw(20) <<  "mdl_g1 " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_g1 << endl;
  cout << setw(20) <<  "mdl_gw " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_gw << endl;
  cout << setw(20) <<  "mdl_vev " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_vev << endl;
  cout << setw(20) <<  "mdl_vev__exp__2 " <<  "= " <<
      setiosflags(ios::scientific) << setw(10) << mdl_vev__exp__2 << endl;
  cout << setw(20) <<  "mdl_lam " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_lam << endl;
  cout << setw(20) <<  "mdl_yb " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_yb << endl;
  cout << setw(20) <<  "mdl_yc " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_yc << endl;
  cout << setw(20) <<  "mdl_ye " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_ye << endl;
  cout << setw(20) <<  "mdl_ym " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_ym << endl;
  cout << setw(20) <<  "mdl_yt " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_yt << endl;
  cout << setw(20) <<  "mdl_ytau " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_ytau << endl;
  cout << setw(20) <<  "mdl_muH " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_muH << endl;
  cout << setw(20) <<  "mdl_I1x31 " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_I1x31 << endl;
  cout << setw(20) <<  "mdl_I1x32 " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_I1x32 << endl;
  cout << setw(20) <<  "mdl_I1x33 " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_I1x33 << endl;
  cout << setw(20) <<  "mdl_I2x12 " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_I2x12 << endl;
  cout << setw(20) <<  "mdl_I2x13 " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_I2x13 << endl;
  cout << setw(20) <<  "mdl_I2x22 " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_I2x22 << endl;
  cout << setw(20) <<  "mdl_I2x23 " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_I2x23 << endl;
  cout << setw(20) <<  "mdl_I2x32 " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_I2x32 << endl;
  cout << setw(20) <<  "mdl_I2x33 " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_I2x33 << endl;
  cout << setw(20) <<  "mdl_I3x21 " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_I3x21 << endl;
  cout << setw(20) <<  "mdl_I3x22 " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_I3x22 << endl;
  cout << setw(20) <<  "mdl_I3x23 " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_I3x23 << endl;
  cout << setw(20) <<  "mdl_I3x31 " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_I3x31 << endl;
  cout << setw(20) <<  "mdl_I3x32 " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_I3x32 << endl;
  cout << setw(20) <<  "mdl_I3x33 " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_I3x33 << endl;
  cout << setw(20) <<  "mdl_I4x13 " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_I4x13 << endl;
  cout << setw(20) <<  "mdl_I4x23 " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_I4x23 << endl;
  cout << setw(20) <<  "mdl_I4x33 " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << mdl_I4x33 << endl;
  cout << setw(20) <<  "mdl_ee__exp__2 " <<  "= " <<
      setiosflags(ios::scientific) << setw(10) << mdl_ee__exp__2 << endl;
  cout << setw(20) <<  "mdl_sw__exp__2 " <<  "= " <<
      setiosflags(ios::scientific) << setw(10) << mdl_sw__exp__2 << endl;
  cout << setw(20) <<  "mdl_cw__exp__2 " <<  "= " <<
      setiosflags(ios::scientific) << setw(10) << mdl_cw__exp__2 << endl;
}
void Parameters_sm_full::printIndependentCouplings()
{
  cout <<  "sm_full model couplings independent of event kinematics:" << endl; 
  cout << setw(20) <<  "GC_83 " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << GC_83 << endl;
}
void Parameters_sm_full::printDependentParameters()
{
  cout <<  "sm_full model parameters dependent on event kinematics:" << endl; 
  cout << setw(20) <<  "mdl_sqrt__aS " <<  "= " << setiosflags(ios::scientific)
      << setw(10) << mdl_sqrt__aS << endl;
  cout << setw(20) <<  "G " <<  "= " << setiosflags(ios::scientific) <<
      setw(10) << G << endl;
  cout << setw(20) <<  "mdl_G__exp__2 " <<  "= " <<
      setiosflags(ios::scientific) << setw(10) << mdl_G__exp__2 << endl;
}
void Parameters_sm_full::printDependentCouplings()
{
  cout <<  "sm_full model couplings dependent on event kinematics:" << endl; 

}

*/
