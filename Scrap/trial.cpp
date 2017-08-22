// Scrapped
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <RcppArmadilloExtensions/sample.h>

using namespace Rcpp;

// Chromosome data storage
typedef struct chromosome {
  DataFrame runDat;
  double Ins;
  double count;
  double fitness;
};

// Function to generate new values of i_number and insertion
double roll(IntegerVector set, double oldval) {
  double _oldval = oldval, newval;
  NumericVector newvect, prob = NumericVector::create();
  bool replace = FALSE;
  IntegerVector _set = set;

  newvect = RcppArmadillo::sample(_set, 1, replace, prob);
  newval = newvect[0];

  if (!_oldval)
    return newval;
  while (newval <= _oldval) {
    newvect = RcppArmadillo::sample(_set, 1, replace, prob);
    newval = newvect[0];
  }
  return newval;
}

// Function to get fitness of model run
double getFit(DataFrame run) {
  DataFrame _run = run;
  Environment global = Environment::global_env();
  Function check = global["edtrial"];
  // Must force SEXP to double
  return as<double>(check(_run));
}

// [[Rcpp::export]]
NumericVector trialrun(Environment env, String age) {
  // Run batch run function and iterate
  Environment global = Environment::global_env();
  Environment _env = env;
  String _age = age;

  const int tf = 3650;

  Function runBatch = global["brtrial"];
  Function epiCheck = global["edtrial"];

  // Some parameter sets here
  IntegerVector infCount = seq_len(100);
  IntegerVector insertion = seq_len(3000);
  const int occ = 12;
  double newCount, newIns, oldCount = 0, oldIns = 0;
  NumericVector out(3);
  out.names() =
      CharacterVector::create("Insertion Time", "Infected Count", "Occurances");

  bool successone;
  do {
    newIns = roll(insertion, oldIns);
    newCount = roll(infCount, oldCount);
    // Rewrite whole thing?
    DataFrame run = runBatch(_env, newIns, newCount, occ, _age, tf);
    successone = epiCheck(run);
  } while (!successone);

  // First Sucesss
  bool successtwo = 1;
  bool replace = FALSE;
  NumericVector newvect(0);
  out["Insertion Time"] = newIns;
  out["Infected Count"] = newCount;
  out["Occurances"] = occ;
  oldCount = out["Infected Count"];
  IntegerVector range = seq_len(out["Infected Count"]);

  // Now decrease number of people being inserted to optimize
  do {
    newvect = RcppArmadillo::sample(range, 1, replace, NumericVector::create());
    newCount = newvect[0];
    if (newCount >= oldCount)
      continue;
    DataFrame run = runBatch(env, out["Insertion Time"], newCount,
                             out["Occurances"], _age, tf);
    successtwo = epiCheck(run);
    if (successtwo)
      oldCount = newCount;
  } while (successtwo);
  // Second Success
  // Store the minimized Count data in output vector
  out[1] = newCount;

  // Return output

  return out;
}
