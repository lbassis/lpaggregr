require(inline)
require(Rcpp)
require(RcppArmadillo)

cxxflags=paste("-I",lpaggreg_location,"/include/ -std=c++11", sep="");
libs=paste("-L",lpaggreg_location,"/lib/ -llpaggreg -Wl,-rpath=",lpaggreg_location,"/lib/", sep="");
Sys.setenv("PKG_CXXFLAGS"=cxxflags);
Sys.setenv("PKG_LIBS"=libs);

lpaggreg_src <- '
// Enable C++11
// [[Rcpp::plugins(cpp11)]]

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

#include <iostream>
#include <vector>
#include <map>
#include <memory>
#include <lpaggreg/oqualities.h>
#include <lpaggreg/opartitioner.h>
#include <lpaggreg/ovaluesn.h>  
#include <lpaggreg/hqualities.h>
#include <lpaggreg/hpartitioner.h>
#include <lpaggreg/hvaluesn.h>
#include <lpaggreg/dqualities.h>
#include <lpaggreg/dpartitioner.h>
#include <lpaggreg/dvaluesn.h>  

using namespace Rcpp;
using namespace arma;
using namespace std;
using namespace lpaggreg;


vector<vector<vector<double> > > convertToMicroModel(NumericVector micro){
  NumericVector vecArray(micro);
  IntegerVector arrayDims = vecArray.attr("dim");
  arma::cube cubeArray(vecArray.begin(), arrayDims[0], arrayDims[1], arrayDims[2], false);
  vector<vector<vector<double> > > micromodel;
  for (int i=0; i<arrayDims[0]; i++){
    micromodel.push_back(vector<vector<double> >());
    for (int j=0; j<arrayDims[1]; j++){
      (micromodel[i]).push_back(vector<double>());
      for (int k=0; k<arrayDims[2]; k++){
        (micromodel[i][j]).push_back(cubeArray(i, j, k));
      }
    }
  }
  return micromodel;
}

vector<int > convertToHierarchy(NumericVector h){
  NumericVector vecArray(h);
  vector<int> hierarchy;
  for (int i=0; i<vecArray.size(); i++){
    hierarchy.push_back(vecArray(i)-1);
  }
  return hierarchy;
}

// [[Rcpp::export]]
List oaggregate(NumericVector micro, SEXP th) {
  float threshold=as<float>(th);
  shared_ptr<OValuesN3> values = shared_ptr<OValuesN3>(new OValuesN3(convertToMicroModel(micro)));
  OQualities qualities = OQualities(values);
  qualities.computeQualities();
  qualities.normalize();
  OPartitioner partitioner = OPartitioner(qualities);
  partitioner.computeBestPartitions(threshold);
  list< tuple<float, int, int, lp_quality_type, lp_quality_type> > partitionsTuples=partitioner.getPartitionsTuples();
  NumericVector parameters(partitionsTuples.size());
  IntegerVector start(partitionsTuples.size());
  IntegerVector end(partitionsTuples.size());
  NumericVector gain(partitionsTuples.size());
  NumericVector loss(partitionsTuples.size());
  int i=0;
  for (tuple<float, int, int, lp_quality_type, lp_quality_type> line: partitionsTuples){
    parameters[i]=get<0>(line);
    start[i]=get<1>(line)+1;
    end[i]=get<2>(line)+1;
    gain[i]=get<3>(line);
    loss[i]=get<4>(line);
    i++;
  }
  DataFrame results = DataFrame::create(Named("Parameter")=parameters,
  Named("Start")=start,
  Named("End")=end,
  Named("Gain")=gain,
  Named("Loss")=loss);
  map<float, shared_ptr<Quality> > qualities2=partitioner.getQualityList();
  NumericVector parameters2(qualities2.size());
  NumericVector gain2(qualities2.size());
  NumericVector loss2(qualities2.size());
  i=0;
  for(auto it: qualities2){
    parameters2[i]=it.first;
    gain2[i]=it.second->getGain();
    loss2[i]=it.second->getLoss();
    i++;
  }
  DataFrame qualitydf = DataFrame::create(Named("Parameter")=parameters2,
  Named("Gain")=gain2,
  Named("Loss")=loss2);
  float popt=partitioner.getP(P_OPT);
  float auc=partitioner.computeAUC();
  return Rcpp::List::create(Named("Partitions") = results, Named("Qualities")= qualitydf, Named("POpt") = popt, Named("AUC") = auc);
}

// [[Rcpp::export]]
List haggregate(NumericVector micro, NumericVector h, SEXP th) {
  float threshold=as<float>(th);
  shared_ptr<HValuesN3> values = shared_ptr<HValuesN3>(new HValuesN3(convertToMicroModel(micro),convertToHierarchy(h)));
  HQualities qualities = HQualities(values);
  qualities.computeQualities();
  qualities.normalize();
  HPartitioner partitioner = HPartitioner(qualities);
  partitioner.computeBestPartitions(threshold);
  list< tuple<float, int, int, lp_quality_type, lp_quality_type> > partitionsTuples=partitioner.getPartitionsTuples();
  NumericVector parameters(partitionsTuples.size());
  IntegerVector node(partitionsTuples.size());
  IntegerVector size(partitionsTuples.size());
  NumericVector gain(partitionsTuples.size());
  NumericVector loss(partitionsTuples.size());
  int i=0;
  for (tuple<float, int, int, lp_quality_type, lp_quality_type> line: partitionsTuples){
    parameters[i]=get<0>(line);
    node[i]=get<1>(line)+1;
    size[i]=get<2>(line);
    gain[i]=get<3>(line);
    loss[i]=get<4>(line);
    i++;
  }
  DataFrame results = DataFrame::create(Named("Parameter")=parameters,
  Named("Node")=node,
  Named("Size")=size,
  Named("Gain")=gain,
  Named("Loss")=loss);
  map<float, shared_ptr<Quality> > qualities2=partitioner.getQualityList();
  NumericVector parameters2(qualities2.size());
  NumericVector gain2(qualities2.size());
  NumericVector loss2(qualities2.size());
  i=0;
  for(auto it: qualities2){
    parameters2[i]=it.first;
    gain2[i]=it.second->getGain();
    loss2[i]=it.second->getLoss();
    i++;
  }
  DataFrame qualitydf = DataFrame::create(Named("Parameter")=parameters2,
  Named("Gain")=gain2,
  Named("Loss")=loss2);
  float popt=partitioner.getP(P_OPT);
  float auc=partitioner.computeAUC();
  return Rcpp::List::create(Named("Partitions") = results, Named("Qualities")= qualitydf, Named("POpt") = popt, Named("AUC") = auc);
}

// [[Rcpp::export]]
List daggregate(NumericVector micro, NumericVector h, SEXP th) {
  float threshold=as<float>(th);
  shared_ptr<DValuesN3> values = shared_ptr<DValuesN3>(new DValuesN3(convertToMicroModel(micro),convertToHierarchy(h)));
  DQualities qualities = DQualities(values);
  qualities.computeQualities();
  qualities.normalize();
  DPartitioner partitioner = DPartitioner(qualities);
  partitioner.computeBestPartitions(threshold);
  list< tuple<float, int, int, int, int, lp_quality_type, lp_quality_type> > partitionsTuples=partitioner.getPartitionsTuples();
  NumericVector parameters(partitionsTuples.size());
  IntegerVector node(partitionsTuples.size());
  IntegerVector size(partitionsTuples.size());
  IntegerVector start(partitionsTuples.size());
  IntegerVector end(partitionsTuples.size());
  NumericVector gain(partitionsTuples.size());
  NumericVector loss(partitionsTuples.size());
  int i=0;
  for (tuple<float, int, int, int, int, lp_quality_type, lp_quality_type> line: partitionsTuples){
    parameters[i]=get<0>(line);
    node[i]=get<1>(line)+1;
    size[i]=get<2>(line);
    start[i]=get<3>(line)+1;
    end[i]=get<4>(line)+1;
    gain[i]=get<5>(line);
    loss[i]=get<6>(line);
    i++;
  }
  DataFrame results = DataFrame::create(Named("Parameter")=parameters,
  Named("Node")=node,
  Named("Size")=size,
  Named("Start")=start,
  Named("End")=end,
  Named("Gain")=gain,
  Named("Loss")=loss);
  map<float, shared_ptr<Quality> > qualities2=partitioner.getQualityList();
  NumericVector parameters2(qualities2.size());
  NumericVector gain2(qualities2.size());
  NumericVector loss2(qualities2.size());
  i=0;
  for(auto it: qualities2){
    parameters2[i]=it.first;
    gain2[i]=it.second->getGain();
    loss2[i]=it.second->getLoss();
    i++;
  }
  DataFrame qualitydf = DataFrame::create(Named("Parameter")=parameters2,
  Named("Gain")=gain2,
  Named("Loss")=loss2);
  float popt=partitioner.getP(P_OPT);
  float auc=partitioner.computeAUC();
  return Rcpp::List::create(Named("Partitions") = results, Named("Qualities")= qualitydf, Named("POpt") = popt, Named("AUC") = auc);
}
'

sourceCpp(code=lpaggreg_src, verbose=TRUE, rebuild=TRUE)