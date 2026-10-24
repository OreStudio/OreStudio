# -*- mode: python; tab-width: 4; indent-tabs-mode: nil -*-
#
# Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
#
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51
# Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
"""The Setup parameters of the engine settings, with the text a screen shows for each.

Every entry is one parameter of the Setup block of ore.xml that the official ORE
documentation describes as an engine choice. The words come from the engine settings
prototype. name is the parameter as ORE writes it. column is the column of the engine
settings part table that holds it. group names the part. introduced_in and removed_in
are measured from the release tags of the ORE source, and removed_in is None while the
parameter exists. The seed script writes one ore parameter for each entry.
"""

SETUP_PARAMETERS = [
    {
        "group": "fixings",
        "name": "implyTodaysFixings",
        "column": "imply_todays_fixings",
        "domain": "boolean",
        "label": "Imply today’s fixings",
        "help": "Whether today’s fixings are implied.",
        "description": (
            "Decides what the run does about the fixing of the valuation date. When it is No, the fixing for the valuation date is loaded from the fixing file like any earlier fixing. When it is Yes, that fixing is not loaded and the engine implies it from the curves. Use Yes when the run prices off hypothetical market data, as in scenario analysis and stress testing, because a fixing from the real market would not match the hypothetical curves."
        ),
        "default_text": "No. Today’s fixing is loaded.",
        "format": "A flag, written Y or N, or true or false.",
        "introduced_in": "1.8.0.7 or earlier",
        "removed_in": None,
    },
    {
        "group": "fixings",
        "name": "includeTodaysCashFlows",
        "column": "include_todays_cash_flows",
        "domain": "boolean",
        "label": "Include today’s cash flows",
        "help": "Whether cash flows that pay today count.",
        "description": (
            "Decides whether a cash flow that pays on the valuation date is part of the NPV or is treated as matured and excluded. It is one of two QuantLib flags that ORE exposes. A choice here applies to every analytic of the run. The simulation analytic has its own parameter of the same name, which applies to the XVA, XVA sensitivity and XVA stress analytics. Change it only when the NPV must agree with a system that treats payments on the valuation date the other way."
        ),
        "default_text": "No. A cash flow that pays on the valuation date is treated as matured.",
        "format": "A flag, written Y or N, or true or false.",
        "introduced_in": "1.8.13.0",
        "removed_in": None,
    },
    {
        "group": "fixings",
        "name": "includeReferenceDateEvents",
        "column": "include_reference_date_events",
        "domain": "boolean",
        "label": "Include reference date events",
        "help": "Whether events on the reference date count.",
        "description": (
            "Decides whether an event that falls on the valuation date is still counted in the NPV or is treated as matured and excluded. It is the companion of Include today’s cash flows, and the two together decide how a trade is valued on the day of its payment or event. A choice here applies to every analytic of the run, and the simulation analytic has its own parameter of the same name."
        ),
        "default_text": "No. An event on the valuation date is treated as matured.",
        "format": "A flag, written Y or N, or true or false.",
        "introduced_in": "1.8.13.0",
        "removed_in": None,
    },
    {
        "group": "fixings",
        "name": "enrichIndexFixings",
        "column": "enrich_index_fixings",
        "domain": "boolean",
        "label": "Enrich index fixings",
        "help": "Whether gaps in index fixings are filled.",
        "description": (
            "Fills gaps in the history of an index. When it is Yes, a missing fixing is replaced by a fallback: the previous fixing first, and the next fixing when there is no previous one. Ignore fixing lag and Ignore fixing lead limit how far the fallback may be from the gap. Use it when the fixing file has gaps. It changes the numbers, because a trade that reads a filled fixing is valued with it."
        ),
        "default_text": "No. A gap stays a gap.",
        "format": "A flag, written Y or N, or true or false.",
        "introduced_in": "1.8.13.0",
        "removed_in": None,
    },
    {
        "group": "fixings",
        "name": "ignoreFixingLag",
        "column": "ignore_fixing_lag",
        "domain": "integer",
        "label": "Ignore fixing lag",
        "help": "A number of days. Zero turns the check off.",
        "description": (
            "Works with Enrich index fixings. When the engine fills a gap in an index’s fixings with the previous fixing, this setting limits how old that previous fixing may be. If the previous fixing is more days older than the gap than this number, the engine does not use it as the fallback. A value of zero turns the check off, so a fixing of any age can fill the gap."
        ),
        "default_text": "0. The check is off.",
        "format": "A whole number of days, zero or more.",
        "introduced_in": "1.8.13.0",
        "removed_in": None,
    },
    {
        "group": "fixings",
        "name": "ignoreFixingLead",
        "column": "ignore_fixing_lead",
        "domain": "integer",
        "label": "Ignore fixing lead",
        "help": "A number of days. Zero turns the check off.",
        "description": (
            "Works with Enrich index fixings. When the engine fills a gap in an index’s fixings with the next fixing, this setting limits how far ahead that next fixing may be. If the next fixing date leads the gap by more days than this number, the engine does not use it as the fallback. A value of zero turns the check off, so a fixing of any distance can fill the gap."
        ),
        "default_text": "0. The check is off.",
        "format": "A whole number of days, zero or more.",
        "introduced_in": "1.8.13.0",
        "removed_in": None,
    },
    {
        "group": "building",
        "name": "observationModel",
        "column": "observation_model",
        "domain": "string",
        "label": "Observation model",
        "help": "How the engine handles QuantLib notifications.",
        "description": (
            "Controls how many QuantLib observer notifications the engine uses when market data and fixings are updated and when the evaluation date moves. Notifications keep every dependent object up to date, and they cost time in a simulation. None is the normal QuantLib behaviour, where every update notifies every dependent object. The other three choices cut that cost in different ways and give a similar speed up. Disable switches all notifications off during market and fixing updates and when the evaluation date moves along a path, and the engine triggers the needed recalculations itself. Defer switches notifications off during market and fixing updates and runs the updates when notifications are on again. Unregister keeps notifications on but unregisters floating rate coupons from their indices, which removes most of them. The ORE user guide describes the setting as a control of performance during simulation."
        ),
        "default_text": "None.",
        "format": "One of None, Disable, Defer, Unregister.",
        "introduced_in": "1.8.0.7 or earlier",
        "removed_in": None,
    },
    {
        "group": "building",
        "name": "lazyMarketBuilding",
        "column": "lazy_market_building",
        "domain": "boolean",
        "label": "Lazy market building",
        "help": "Whether the market is built on demand.",
        "description": (
            "Decides when the engine builds the curves listed in today’s market. When it is Yes, a curve is built only when a trade or analytic asks for it, which saves time when the configuration lists curves that the portfolio never uses. When it is No, every curve in today’s market is built at the start."
        ),
        "default_text": "Yes. Curves are built on demand.",
        "format": "A flag, written Y or N, or true or false.",
        "introduced_in": "1.8.6.0",
        "removed_in": None,
    },
    {
        "group": "building",
        "name": "buildFailedTrades",
        "column": "build_failed_trades",
        "domain": "boolean",
        "label": "Build failed trades",
        "help": "Whether a failed trade is replaced by a dummy.",
        "description": (
            "Decides what happens to a trade that cannot be loaded or built. When it is Yes, the engine builds a dummy trade of type Failed, with zero notional and zero NPV, so the trade still appears in the portfolio. When it is No, the failed trade is left out. Yes keeps the trade count honest, but a reader must know that a Failed trade has no value."
        ),
        "default_text": "No. A failed trade is left out. The ORE application sets this default; the engine library itself defaults to Yes.",
        "format": "A flag, written Y or N, or true or false.",
        "introduced_in": "1.8.7.0",
        "removed_in": None,
    },
    {
        "group": "building",
        "name": "continueOnError",
        "column": "continue_on_error",
        "domain": "boolean",
        "label": "Continue on error",
        "help": "Whether the run goes on after an error.",
        "description": (
            "Decides whether the engine stops at the first error or goes on. When it is Yes, the engine does not exit on an error but tries to continue the processing, so one bad curve or trade does not lose the whole run, and the reports may be incomplete. When it is No, the run exits at the error."
        ),
        "default_text": "Yes. The run goes on after an error.",
        "format": "A flag, written Y or N, or true or false.",
        "introduced_in": "1.8.4.0",
        "removed_in": None,
    },
    {
        "group": "building",
        "name": "useAtParCouponsCurves",
        "column": "use_at_par_coupons_curves",
        "domain": "boolean",
        "label": "At par coupons for curves",
        "help": "Par approximation or indexed ibor coupons for curves.",
        "description": (
            "Decides how the engine models a floating rate coupon when it builds the curves. When it is Yes, it uses the par approximation of the coupon. When it is No, it uses indexed ibor coupons. The choice goes back to the QuantLib flag QL_USE_INDEXED_COUPON and its run time setting. Yes is also the default of a QuantLib build."
        ),
        "default_text": "Yes. The par approximation.",
        "format": "A flag, written Y or N, or true or false.",
        "introduced_in": "1.8.15.0",
        "removed_in": None,
    },
    {
        "group": "building",
        "name": "useAtParCouponsTrades",
        "column": "use_at_par_coupons_trades",
        "domain": "boolean",
        "label": "At par coupons for trades",
        "help": "Par approximation or indexed ibor coupons for trades.",
        "description": (
            "Decides how the engine models a floating rate coupon when it builds the trades. When it is Yes, it uses the par approximation of the coupon. When it is No, it uses indexed ibor coupons. It is the companion of At par coupons for curves, and the two can differ. The choice goes back to the QuantLib flag QL_USE_INDEXED_COUPON and its run time setting."
        ),
        "default_text": "Yes. The par approximation.",
        "format": "A flag, written Y or N, or true or false.",
        "introduced_in": "1.8.15.0",
        "removed_in": None,
    },
    {
        "group": "execution",
        "name": "nThreads",
        "column": "n_threads",
        "domain": "integer",
        "label": "Threads",
        "help": "A whole number of at least 1.",
        "description": (
            "The number of threads the engine may use. More than one thread speeds up the analytics that spread their work over scenarios or paths: the sensitivity analytic, the classic exposure simulation and the AMC exposure simulation. These are the three analytics that the ORE user guide names."
        ),
        "default_text": "1.",
        "format": "A whole number, at least 1.",
        "introduced_in": "1.8.9.0",
        "removed_in": None,
    },
    {
        "group": "execution",
        "name": "logMask",
        "column": "log_mask",
        "domain": "integer",
        "label": "Log mask",
        "help": "A bit set, such as 31.",
        "description": (
            "Decides which messages the engine writes to its log file. Each kind of message has a bit: 1 alert, 2 critical, 4 error, 8 warning, 16 notice, 32 debug and 64 data. Add the bits of the kinds you want. Common totals are 15 for alert to warning, 31 for the same and notice, 63 for the same and debug, and 127 for the same and data. A higher total writes more and makes a larger log. The value 255 gives the most detail."
        ),
        "default_text": "15. Alert, critical, error and warning.",
        "format": "A whole number. A bit set of the message kinds.",
        "introduced_in": "1.8.2.0",
        "removed_in": None,
    },
    {
        "group": "execution",
        "name": "gzipCompressionLevel",
        "column": "gzip_compression_level",
        "domain": "integer",
        "label": "Gzip compression level",
        "help": "A whole number from 0 to 9.",
        "description": (
            "Sets how strongly the engine compresses the zipped cube files it writes. The value is a whole number from 0 to 9. A value of 0 means no compression, which gives the fastest write and the largest file. A value of 9 means the strongest compression, which gives the slowest write and the smallest file. A value outside 0 to 9 is an error. The default of 6 balances speed and size."
        ),
        "default_text": "6. A balance of speed and size.",
        "format": "A whole number from 0 to 9.",
        "introduced_in": "1.8.17.0",
        "removed_in": None,
    },
]
