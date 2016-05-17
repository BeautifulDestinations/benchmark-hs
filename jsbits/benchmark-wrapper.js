
var Benchmark = require('benchmark');
var process = require('process');
var benchmark = require('benchmark');
var _ = require('lodash');

// if (typeof window !== 'undefined') {
//   benchmark = benchmark.runInContext({_, process});
//   window.Benchmark = benchmark;
// }

function bddebug(a) {
	if (window.beautifulDestinationsDebug) {
    window.beautifulDestinationsDebug(a);
  }
}

/* Any native JS stuff you want here */

module.exports =
	{ Benchmark : Benchmark
	};
h$benchmark = module.exports;
