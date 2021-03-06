// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// rcpprelu
arma::mat rcpprelu(const arma::mat& x);
RcppExport SEXP _SRCL_rcpprelu(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpprelu(x));
    return rcpp_result_gen;
END_RCPP
}
// rcpprelu_neg
arma::mat rcpprelu_neg(const arma::mat& x);
RcppExport SEXP _SRCL_rcpprelu_neg(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpprelu_neg(x));
    return rcpp_result_gen;
END_RCPP
}
// SRCL_cpp_train_network_relu
Rcpp::List SRCL_cpp_train_network_relu(const arma::mat& x, const arma::vec& y, const arma::mat& testx, const arma::vec& testy, const arma::mat& W1_input, const arma::mat& B1_input, const arma::mat& W2_input, const arma::mat& B2_input, const arma::vec& IPCW, double lr, double maxepochs, double L1);
RcppExport SEXP _SRCL_SRCL_cpp_train_network_relu(SEXP xSEXP, SEXP ySEXP, SEXP testxSEXP, SEXP testySEXP, SEXP W1_inputSEXP, SEXP B1_inputSEXP, SEXP W2_inputSEXP, SEXP B2_inputSEXP, SEXP IPCWSEXP, SEXP lrSEXP, SEXP maxepochsSEXP, SEXP L1SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type y(ySEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type testx(testxSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type testy(testySEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type W1_input(W1_inputSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type B1_input(B1_inputSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type W2_input(W2_inputSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type B2_input(B2_inputSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type IPCW(IPCWSEXP);
    Rcpp::traits::input_parameter< double >::type lr(lrSEXP);
    Rcpp::traits::input_parameter< double >::type maxepochs(maxepochsSEXP);
    Rcpp::traits::input_parameter< double >::type L1(L1SEXP);
    rcpp_result_gen = Rcpp::wrap(SRCL_cpp_train_network_relu(x, y, testx, testy, W1_input, B1_input, W2_input, B2_input, IPCW, lr, maxepochs, L1));
    return rcpp_result_gen;
END_RCPP
}
// SRCL_cpp_train_network_relu_with_confounder
Rcpp::List SRCL_cpp_train_network_relu_with_confounder(const arma::mat& x, const arma::vec& y, const arma::mat& c, const arma::mat& testx, const arma::vec& testy, const arma::mat& testc, const arma::mat& W1_input, const arma::mat& B1_input, const arma::mat& W2_input, const arma::mat& B2_input, const arma::mat& C2_input, double lr, double maxepochs);
RcppExport SEXP _SRCL_SRCL_cpp_train_network_relu_with_confounder(SEXP xSEXP, SEXP ySEXP, SEXP cSEXP, SEXP testxSEXP, SEXP testySEXP, SEXP testcSEXP, SEXP W1_inputSEXP, SEXP B1_inputSEXP, SEXP W2_inputSEXP, SEXP B2_inputSEXP, SEXP C2_inputSEXP, SEXP lrSEXP, SEXP maxepochsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type y(ySEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type c(cSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type testx(testxSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type testy(testySEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type testc(testcSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type W1_input(W1_inputSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type B1_input(B1_inputSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type W2_input(W2_inputSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type B2_input(B2_inputSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type C2_input(C2_inputSEXP);
    Rcpp::traits::input_parameter< double >::type lr(lrSEXP);
    Rcpp::traits::input_parameter< double >::type maxepochs(maxepochsSEXP);
    rcpp_result_gen = Rcpp::wrap(SRCL_cpp_train_network_relu_with_confounder(x, y, c, testx, testy, testc, W1_input, B1_input, W2_input, B2_input, C2_input, lr, maxepochs));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_SRCL_rcpprelu", (DL_FUNC) &_SRCL_rcpprelu, 1},
    {"_SRCL_rcpprelu_neg", (DL_FUNC) &_SRCL_rcpprelu_neg, 1},
    {"_SRCL_SRCL_cpp_train_network_relu", (DL_FUNC) &_SRCL_SRCL_cpp_train_network_relu, 12},
    {"_SRCL_SRCL_cpp_train_network_relu_with_confounder", (DL_FUNC) &_SRCL_SRCL_cpp_train_network_relu_with_confounder, 13},
    {NULL, NULL, 0}
};

RcppExport void R_init_SRCL(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
