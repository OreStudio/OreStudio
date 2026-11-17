/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */

/**
 * @brief Writes the generated halves of the market data vendor instrument seed.
 *
 * The seed installs one vendor instrument for each instrument the synthetic feeds quote, in the
 * notation of the quote's own provider, and maps it to the oresmd item the quote names. Two
 * markets are quoted today: the spot quote of a currency pair, and one pillar of an interest rate
 * curve. Neither half of either row is written by hand -- the external id is what the provider's
 * notation writes for the quote, and the item is the datum the quote names -- so this program is
 * the one writer of both, and the ids in the seed and the ids the runtime writes cannot drift.
 *
 * The input is one line for each quote of each artefact config, from the populate script that
 * reads them. The first field names the market:
 *
 *     fx <TAB> provider_code <TAB> base_currency_code <TAB> quote_currency_code
 *     ir <TAB> provider_code <TAB> currency_code <TAB> index_family <TAB> instrument_code <TAB>
 *        start_tenor_code <TAB> end_tenor_code <TAB> tenor
 *
 * @c tenor is the curve config's index tenor field, which decides the series the pillar lands in;
 * an empty value and the meeting-dated FOMC value both name an overnight index. A notation whose
 * ids name that tenor is given it, and one whose ids do not is given none. The output is the two
 * SQL statements that fill the seed's lookup tables, which the seed joins to the artefact tables at
 * populate time.
 *
 * A quote the provider's notation cannot write -- a meeting-dated pillar is spelled with codes no
 * notation writes, and a provider with no notation writes nothing at all -- gets a row with the
 * four id columns null, so the seed can tell "no id for this quote" from "this quote is new and
 * the seed is stale".
 *
 * Usage, from the repository root -- the query is the input, written to a file by psql, so no
 * shell pipeline runs the compass command that produces it:
 *
 *     ./compass.sh sql -- -t -A -F $'\t' -o tmp/vendor-catalogue.tsv -c "<the query below>"
 *     build/publish/bin/ores.marketdata.vendor-catalogue --write tmp/vendor-catalogue.tsv
 *
 * The query, which the seed's header repeats:
 *
 *     select 'fx', c.provider_code, c.base_currency_code, c.quote_currency_code, '', '', '', ''
 *     from ores_dq_fx_spot_generation_configs_artefact_tbl c
 *     where c.provider_code is not null
 *     union all
 *     select 'ir', c.provider_code, c.currency_code, c.index_family, e.instrument_code,
 *            e.start_tenor_code, e.end_tenor_code, c.tenor
 *     from ores_dq_ir_curve_generation_configs_artefact_tbl c
 *     join ores_dq_ir_curve_template_entries_artefact_tbl e
 *       on e.dataset_id = c.dataset_id
 *      and e.currency_code = c.currency_code
 *      and e.index_family = c.index_family
 *      and e.tenor = c.tenor
 *     where c.provider_code is not null
 *     union all
 *     select 'vol', c.provider_code, f.base_currency_code, f.quote_currency_code, e.tenor,
 *            '', '', ''
 *     from ores_dq_fx_vol_surface_feeds_artefact_tbl f
 *     join ores_dq_fx_vol_surface_feed_expiries_artefact_tbl e
 *       on e.dataset_id = f.dataset_id and e.feed_id = f.id
 *     join ores_dq_fx_vol_surface_feed_contributors_artefact_tbl l
 *       on l.dataset_id = f.dataset_id and l.feed_id = f.id
 *     join ores_dq_fx_vol_contributors_artefact_tbl c
 *       on c.dataset_id = l.dataset_id and c.id = l.contributor_id
 *
 * A surface's line is expanded to the five labels its feed quotes at the expiry, and its rows go
 * to the volatility seed, which runs after the surfaces are staged.
 *
 * With no --write the statements go to stdout. With no input file the quotes are read from stdin.
 */
#include "ores.marketdata.api/datum/market_datum.hpp"
#include "ores.marketdata.api/datum/oresmd_uri_codec.hpp"
#include "ores.marketdata.core/oresmd/pillar_quote.hpp"
#include "ores.marketdata.core/vendor/provider_notation.hpp"
#include "ores.marketdata.core/vendor/quote_description.hpp"
#include "ores.synthetic.api/domain/ir_curve_generation_config.hpp"
#include "ores.synthetic.api/feeds/ir_curve_template_resolver.hpp"
#include <algorithm>
#include <array>
#include <chrono>
#include <cstddef>
#include <filesystem>
#include <format>
#include <fstream>
#include <iostream>
#include <map>
#include <optional>
#include <sstream>
#include <stdexcept>
#include <string>
#include <tuple>
#include <vector>

namespace {

namespace vendor = ores::marketdata::vendor;
namespace core = ores::marketdata::core;
namespace datum = ores::marketdata::datum;

/// The line pairs the program owns in the seed. Everything between each pair is rewritten.
constexpr std::string_view fx_begin_marker = "-- BEGIN GENERATED fx pair ids";
constexpr std::string_view fx_end_marker = "-- END GENERATED fx pair ids";
constexpr std::string_view ir_begin_marker = "-- BEGIN GENERATED ir pillar ids";
constexpr std::string_view ir_end_marker = "-- END GENERATED ir pillar ids";
constexpr std::string_view vol_begin_marker = "-- BEGIN GENERATED fx option ids";
constexpr std::string_view vol_end_marker = "-- END GENERATED fx option ids";

/// The seed of the volatility points, which runs after the surfaces are staged.
constexpr std::string_view default_option_seed =
    "projects/ores.sql/populate/marketdata/marketdata_vendor_volatility_instruments_populate.sql";

/// The labels a surface feed quotes at each expiry.
constexpr std::array<std::string_view, 5> option_labels{"ATM", "25RR", "25BF", "10RR", "10BF"};

/// The seed the program writes to when it is not told otherwise.
constexpr std::string_view default_seed =
    "projects/ores.sql/populate/marketdata/marketdata_vendor_instruments_populate.sql";

/// One spot quote of a currency pair, as the artefact tables state it.
struct pair_feed final {
    std::string provider_code;
    std::string base_currency_code;
    std::string quote_currency_code;
};

/// One pillar of one curve, as the artefact tables state it.
struct pillar_feed final {
    std::string provider_code;
    std::string currency_code;
    std::string index_family;
    std::string instrument_code;
    std::string start_tenor_code;
    std::string end_tenor_code;
    /// The config's index tenor field: empty for an overnight index, FOMC for the meeting-dated
    /// variant of one, or a period such as 3M for a term index.
    std::string tenor;
};

/// One point of one surface a contributor quotes, as the artefact tables state it.
struct option_feed final {
    std::string provider_code;
    std::string base_currency_code;
    std::string quote_currency_code;
    std::string expiry;
    std::string strike_label;
};

/// The id and the item of one quote, or neither when no notation writes the quote.
struct quote_row final {
    std::optional<ores::marketdata::messaging::vendor_id> id;
    std::optional<std::string> oresmd_uri;
};

/// A market's rows, kept in one order so a rerun over the same quotes writes the same file.
template <typename Feed>
using row_map = std::map<Feed, quote_row, bool (*)(const Feed&, const Feed&)>;

/// Splits a line on tabs, keeping empty fields: a pillar whose index tenor field is empty still
/// has one. A trailing carriage return is dropped, so a file written on Windows reads the same.
std::vector<std::string> split_tabs(std::string line) {
    if (!line.empty() && line.back() == '\r')
        line.pop_back();
    std::vector<std::string> out;
    std::string::size_type start = 0;
    for (;;) {
        const auto tab = line.find('\t', start);
        if (tab == std::string::npos) {
            out.push_back(line.substr(start));
            return out;
        }
        out.push_back(line.substr(start, tab - start));
        start = tab + 1;
    }
}

/// @p text as a SQL string literal.
std::string quoted(const std::string& text) {
    std::string out;
    out.reserve(text.size() + 2);
    out.push_back('\'');
    for (const auto c : text) {
        if (c == '\'')
            out.push_back('\'');
        out.push_back(c);
    }
    out.push_back('\'');
    return out;
}

/// The four id columns of a row: the id the notation wrote and the item it names, or four nulls
/// for a quote no notation writes.
std::string id_columns_of(const quote_row& row) {
    if (!row.id)
        return "null, null, null, null";
    return std::format("{}, {}, {}, {}",
                       quoted(row.id->id_scheme),
                       quoted(row.id->external_id),
                       quoted(row.id->field),
                       quoted(*row.oresmd_uri));
}

/// Fails when @p written does not read back as the quote it was written for.
///
/// A notation writes what it reads: an id that reads back as another quote would map another
/// quote's ticks to this item.
void require_round_trip(const vendor::provider_notation& notation,
                        const vendor::quote_description& description,
                        const ores::marketdata::messaging::vendor_id& written) {
    const auto read = notation.read(written);
    if (!read || !(*read == description))
        throw std::runtime_error("the notation of " + std::string(notation.provider()) +
                                 " wrote '" + written.external_id +
                                 "' for a quote it does not read back");
}

/// The oresmd item of a currency pair's spot quote: the datum the pair names.
std::string pair_datum_uri(const pair_feed& p) {
    const auto made = datum::market_datum::make(datum::instrument_type::fx_spot,
                                                datum::quote_type::rate,
                                                {{datum::field::unit_ccy, p.base_currency_code},
                                                 {datum::field::ccy, p.quote_currency_code}});
    if (!made)
        throw std::runtime_error("no datum for the pair " + p.base_currency_code + "/" +
                                 p.quote_currency_code + ": " + made.error());
    const auto uri = datum::oresmd_uri_codec::write(*made);
    if (!uri)
        throw std::runtime_error("no item for the pair " + p.base_currency_code + "/" +
                                 p.quote_currency_code + ": " + uri.error());
    return *uri;
}

/// The oresmd item the pillar's quote is.
///
/// The pillar's own tenors and the curve's index tenor name the quote, so neither date is part of
/// it: a pillar that starts at spot holds 0D and one that starts at a period holds that period.
/// The dates are passed because the builder takes them, and the pillar's term is the tenor its
/// template names rather than a date, which is the case no notation refuses.
std::string pillar_datum_uri(const pillar_feed& p, const std::string& index_tenor) {
    const core::pillar_quote quote{p.currency_code,
                                   index_tenor,
                                   p.start_tenor_code,
                                   p.end_tenor_code,
                                   std::chrono::year{2026} / 1 / 1,
                                   std::chrono::year{2026} / 1 / 2};
    const auto made = core::pillar_quote_datum(quote);
    if (!made)
        throw std::runtime_error("no datum for the pillar " + p.currency_code + " " +
                                 p.start_tenor_code + " to " + p.end_tenor_code + ": " +
                                 made.error());
    const auto uri = datum::oresmd_uri_codec::write(*made);
    if (!uri)
        throw std::runtime_error("no item for the pillar " + p.currency_code + " " +
                                 p.start_tenor_code + " to " + p.end_tenor_code + ": " +
                                 uri.error());
    return *uri;
}

/// The oresmd item of a surface point: the fx_option datum of the pair, the expiry and the label,
/// which is the item the blueprints' pillars read.
std::string option_datum_uri(const option_feed& p) {
    const auto expiry = datum::term::parse(p.expiry);
    const auto label = datum::strike_label::parse(p.strike_label);
    if (!expiry || !label)
        throw std::runtime_error("no datum for the point " + p.base_currency_code + "/" +
                                 p.quote_currency_code + " " + p.expiry + " " + p.strike_label +
                                 ": " + (!expiry ? expiry.error() : label.error()));
    const auto made = datum::market_datum::make(datum::instrument_type::fx_option,
                                                datum::quote_type::rate_lnvol,
                                                {{datum::field::unit_ccy, p.base_currency_code},
                                                 {datum::field::ccy, p.quote_currency_code},
                                                 {datum::field::expiry, *expiry},
                                                 {datum::field::strike_label, *label}});
    if (!made)
        throw std::runtime_error("no datum for the point " + p.base_currency_code + "/" +
                                 p.quote_currency_code + " " + p.expiry + " " + p.strike_label +
                                 ": " + made.error());
    const auto uri = datum::oresmd_uri_codec::write(*made);
    if (!uri)
        throw std::runtime_error("no item for the point " + p.base_currency_code + "/" +
                                 p.quote_currency_code + " " + p.expiry + " " + p.strike_label +
                                 ": " + uri.error());
    return *uri;
}

/// The row for one surface point: the provider's id and the point's item, or null for both when
/// the provider cannot name the point.
quote_row
row_of(const option_feed& p, std::size_t& unnamed_by_notation, std::size_t& unnamed_by_provider) {
    quote_row row;
    const auto* notation = vendor::notation_of(p.provider_code);
    if (notation == nullptr) {
        ++unnamed_by_provider;
        return row;
    }
    const vendor::fx_option_quote description{
        p.base_currency_code, p.quote_currency_code, p.expiry, p.strike_label};
    const auto written = notation->write(description);
    if (!written) {
        std::cerr << "no id: " << p.provider_code << " " << p.base_currency_code << "/"
                  << p.quote_currency_code << " " << p.expiry << " " << p.strike_label << ": "
                  << written.error().message << '\n';
        ++unnamed_by_notation;
        return row;
    }
    require_round_trip(*notation, description, *written);
    row.id = *written;
    row.oresmd_uri = option_datum_uri(p);
    return row;
}

/// The row for one currency pair: the provider's id and the pair's item, or null for both when
/// the provider cannot name the pair.
quote_row
row_of(const pair_feed& p, std::size_t& unnamed_by_notation, std::size_t& unnamed_by_provider) {
    quote_row row;
    const auto* notation = vendor::notation_of(p.provider_code);
    if (notation == nullptr) {
        ++unnamed_by_provider;
        return row;
    }

    const vendor::fx_spot_quote description{
        p.base_currency_code, p.quote_currency_code, notation->primary_side()};
    const auto written = notation->write(description);
    if (!written) {
        std::cerr << "no id: " << p.provider_code << " " << p.base_currency_code << "/"
                  << p.quote_currency_code << ": " << written.error().message << '\n';
        ++unnamed_by_notation;
        return row;
    }

    require_round_trip(*notation, description, *written);
    row.id = *written;
    row.oresmd_uri = pair_datum_uri(p);
    return row;
}

/// The row for one pillar: the provider's id and the pillar's item, or null for both when the
/// provider cannot name the pillar.
quote_row row_of(const pillar_feed& p,
                 const std::string& index_tenor,
                 std::size_t& unnamed_by_notation,
                 std::size_t& unnamed_by_provider) {
    quote_row row;
    const auto* notation = vendor::notation_of(p.provider_code);
    if (notation == nullptr) {
        ++unnamed_by_provider;
        return row;
    }

    vendor::ir_curve_pillar description{
        p.currency_code, p.index_family, p.instrument_code, p.start_tenor_code, p.end_tenor_code};
    // Only a provider whose id names the tenor of the curve's index is given it: another provider
    // would write an id that reads back without it.
    if (notation->names_index_tenor())
        description.index_tenor = index_tenor;
    const auto written = notation->write(description);
    if (!written) {
        std::cerr << "no id: " << p.provider_code << " " << p.currency_code << " " << p.index_family
                  << " " << p.instrument_code << " " << p.start_tenor_code << " "
                  << p.end_tenor_code << ": " << written.error().message << '\n';
        ++unnamed_by_notation;
        return row;
    }

    require_round_trip(*notation, description, *written);
    row.id = *written;
    row.oresmd_uri = pillar_datum_uri(p, index_tenor);
    return row;
}

/// What each provider got, as the comment above a generated statement states it.
template <typename Feed>
std::string counts_comment(const row_map<Feed>& rows) {
    std::map<std::string, std::pair<std::size_t, std::size_t>> by_provider;
    for (const auto& [feed, row] : rows) {
        auto& counts = by_provider[feed.provider_code];
        if (row.id)
            ++counts.first;
        else
            ++counts.second;
    }
    std::string out;
    for (const auto& [provider, counts] : by_provider)
        out += std::format(
            "    -- {}: {} with an id, {} without\n", provider, counts.first, counts.second);
    return out;
}

/// The statement that fills the seed's currency pair lookup, one row for each pair of each
/// provider.
std::string fx_statement_of(const row_map<pair_feed>& rows) {
    std::string out = counts_comment(rows);
    out += "    insert into t_fx_pair_id (provider_code, base_currency_code,\n"
           "        quote_currency_code, id_scheme, external_id, field, oresmd_uri) values\n";
    std::size_t i = 0;
    for (const auto& [feed, row] : rows) {
        out += std::format("    ({}, {}, {}, ",
                           quoted(feed.provider_code),
                           quoted(feed.base_currency_code),
                           quoted(feed.quote_currency_code));
        out += id_columns_of(row);
        out += (++i == rows.size()) ? ");\n" : "),\n";
    }
    return out;
}

/// The statement that fills the seed's pillar lookup, one row for each pillar of each curve of
/// each provider.
std::string ir_statement_of(const row_map<pillar_feed>& rows) {
    std::string out = counts_comment(rows);
    out += "    insert into t_ir_pillar_id (provider_code, currency_code, index_family,\n"
           "        instrument_code, start_tenor_code, end_tenor_code, config_tenor,\n"
           "        id_scheme, external_id, field, oresmd_uri) values\n";
    std::size_t i = 0;
    for (const auto& [feed, row] : rows) {
        out += std::format("    ({}, {}, {}, {}, {}, {}, {}, ",
                           quoted(feed.provider_code),
                           quoted(feed.currency_code),
                           quoted(feed.index_family),
                           quoted(feed.instrument_code),
                           quoted(feed.start_tenor_code),
                           quoted(feed.end_tenor_code),
                           quoted(feed.tenor));
        out += id_columns_of(row);
        out += (++i == rows.size()) ? ");\n" : "),\n";
    }
    return out;
}

/// The statement that fills the volatility seed's lookup, one row for each point of each surface
/// of each contributor.
std::string vol_statement_of(const row_map<option_feed>& rows) {
    std::string out = counts_comment(rows);
    out += "    insert into t_fx_option_id (provider_code, base_currency_code,\n"
           "        quote_currency_code, expiry, strike_label, id_scheme, external_id, field,\n"
           "        oresmd_uri) values\n";
    std::size_t i = 0;
    for (const auto& [feed, row] : rows) {
        out += std::format("    ({}, {}, {}, {}, {}, ",
                           quoted(feed.provider_code),
                           quoted(feed.base_currency_code),
                           quoted(feed.quote_currency_code),
                           quoted(feed.expiry),
                           quoted(feed.strike_label));
        out += id_columns_of(row);
        out += (++i == rows.size()) ? ");\n" : "),\n";
    }
    return out;
}

/// The seed's text with the region between @p begin_marker and @p end_marker replaced by
/// @p statement.
std::string replaced(const std::string& seed,
                     std::string_view begin_marker,
                     std::string_view end_marker,
                     const std::string& statement) {
    const auto begin = seed.find(begin_marker);
    const auto end = seed.find(end_marker);
    if (begin == std::string::npos || end == std::string::npos || end < begin)
        throw std::runtime_error("the seed holds no region '" + std::string(begin_marker) + "'");
    const auto after_begin = seed.find('\n', begin);
    if (after_begin == std::string::npos)
        throw std::runtime_error("the seed's begin marker ends its file");
    return seed.substr(0, after_begin + 1) + statement + seed.substr(end);
}

/// The quotes of the two markets the input states.
struct feeds final {
    std::vector<pair_feed> pairs;
    std::vector<pillar_feed> pillars;
    std::vector<option_feed> options;
};

/// The input, read from @p in: one line for each quote of each artefact config.
feeds read_feeds(std::istream& in) {
    feeds out;
    std::string line;
    while (std::getline(in, line)) {
        if (line.empty())
            continue;
        const auto fields = split_tabs(line);
        if (fields[0] == "fx") {
            // The query pads the pair's line to the pillar's width, because one UNION ALL has one
            // shape, so a pair carries four fields and then as many empty ones as it needs.
            if (fields.size() < 4)
                throw std::runtime_error(
                    "expected at least four tab separated fields for a pair, got '" + line + "'");
            out.pairs.push_back(pair_feed{fields[1], fields[2], fields[3]});
        } else if (fields[0] == "ir") {
            if (fields.size() != 8)
                throw std::runtime_error("expected eight tab separated fields for a pillar, got '" +
                                         line + "'");
            out.pillars.push_back(pillar_feed{
                fields[1], fields[2], fields[3], fields[4], fields[5], fields[6], fields[7]});
        } else if (fields[0] == "vol") {
            // A surface's line names its expiry; each of the labels the feed quotes is a point.
            if (fields.size() < 5)
                throw std::runtime_error(
                    "expected at least five tab separated fields for a surface, got '" + line +
                    "'");
            for (const auto label : option_labels)
                out.options.push_back(
                    option_feed{fields[1], fields[2], fields[3], fields[4], std::string(label)});
        } else {
            throw std::runtime_error("'" + fields[0] + "' is not a market this program writes");
        }
    }
    return out;
}

/// The order of the pairs, and the identity the seed joins on.
bool pair_less(const pair_feed& a, const pair_feed& b) {
    return std::tie(a.provider_code, a.base_currency_code, a.quote_currency_code) <
           std::tie(b.provider_code, b.base_currency_code, b.quote_currency_code);
}

/// The order of the pillars, and the identity the seed joins on: one pillar of one curve of one
/// provider, named the way the config that quotes it names it. Two configs that differ only in
/// the index tenor are two rows, because their pillars land in two series.
bool pillar_less(const pillar_feed& a, const pillar_feed& b) {
    return std::tie(a.provider_code,
                    a.currency_code,
                    a.index_family,
                    a.instrument_code,
                    a.start_tenor_code,
                    a.end_tenor_code,
                    a.tenor) < std::tie(b.provider_code,
                                        b.currency_code,
                                        b.index_family,
                                        b.instrument_code,
                                        b.start_tenor_code,
                                        b.end_tenor_code,
                                        b.tenor);
}


/// The order of the surface points, and the identity the seed joins on.
bool option_less(const option_feed& a, const option_feed& b) {
    return std::tie(a.provider_code,
                    a.base_currency_code,
                    a.quote_currency_code,
                    a.expiry,
                    a.strike_label) < std::tie(b.provider_code,
                                               b.base_currency_code,
                                               b.quote_currency_code,
                                               b.expiry,
                                               b.strike_label);
}
}

int main(int argc, char** argv) {
    auto seed_path = std::filesystem::path(std::string(default_seed));
    auto option_seed_path = std::filesystem::path(std::string(default_option_seed));
    std::optional<std::filesystem::path> feeds_path;
    bool write = false;
    for (int i = 1; i < argc; ++i) {
        const std::string arg = argv[i];
        if (arg == "--write") {
            write = true;
        } else if (arg == "--seed" && i + 1 < argc) {
            seed_path = std::filesystem::path(argv[++i]);
        } else if (arg == "--option-seed" && i + 1 < argc) {
            option_seed_path = std::filesystem::path(argv[++i]);
        } else if (arg.starts_with("--")) {
            std::cerr << "usage: " << argv[0]
                      << " [--seed FILE] [--write] [QUOTES]\n"
                         "  Reads one line for each quote of each artefact config, tab separated:\n"
                         "    fx provider base quote\n"
                         "    ir provider currency index-family instrument start end tenor\n"
                         "    vol provider base quote expiry\n"
                         "  Writes the seeds' three lookup tables.\n";
            return 2;
        } else {
            feeds_path = std::filesystem::path(arg);
        }
    }

    try {
        std::ifstream file;
        std::istream* in = &std::cin;
        if (feeds_path) {
            file.open(*feeds_path);
            if (!file)
                throw std::runtime_error("cannot read the quotes: " + feeds_path->string());
            in = &file;
        }
        const auto input = read_feeds(*in);
        if (input.pairs.empty() || input.pillars.empty())
            throw std::runtime_error(std::string("the input holds no ") +
                                     (input.pairs.empty() ? "pair" : "pillar"));

        std::size_t unnamed_by_notation = 0;
        std::size_t unnamed_by_provider = 0;

        row_map<pair_feed> fx_rows(pair_less);
        for (const auto& p : input.pairs)
            fx_rows.emplace(p, row_of(p, unnamed_by_notation, unnamed_by_provider));

        row_map<pillar_feed> ir_rows(pillar_less);
        for (const auto& p : input.pillars) {
            ores::synthetic::domain::ir_curve_generation_config cfg;
            cfg.currency_code = p.currency_code;
            cfg.index_family = p.index_family;
            cfg.tenor = p.tenor;
            ir_rows.emplace(p,
                            row_of(p,
                                   ores::synthetic::feed::ir_curve_index_tenor(cfg),
                                   unnamed_by_notation,
                                   unnamed_by_provider));
        }

        row_map<option_feed> vol_rows(option_less);
        for (const auto& p : input.options)
            vol_rows.emplace(p, row_of(p, unnamed_by_notation, unnamed_by_provider));

        const auto fx_statement = fx_statement_of(fx_rows);
        const auto ir_statement = ir_statement_of(ir_rows);
        const auto vol_statement = vol_rows.empty() ? std::string() : vol_statement_of(vol_rows);

        if (!write) {
            std::cout << fx_statement << ir_statement << vol_statement;
            return 0;
        }

        // The volatility points have a seed of their own, which runs after the surfaces it reads
        // are staged.
        if (!vol_rows.empty()) {
            std::ifstream option_file(option_seed_path);
            if (!option_file)
                throw std::runtime_error("cannot read the seed: " + option_seed_path.string());
            std::ostringstream option_text;
            option_text << option_file.rdbuf();
            option_file.close();
            const auto option_updated =
                replaced(option_text.str(), vol_begin_marker, vol_end_marker, vol_statement);
            if (option_updated != option_text.str()) {
                std::ofstream out(option_seed_path);
                if (!out)
                    throw std::runtime_error("cannot write the seed: " + option_seed_path.string());
                out << option_updated;
                std::cout << "wrote " << vol_rows.size() << " surface point(s) to "
                          << option_seed_path.string() << '\n';
            }
        }

        std::ifstream seed_file(seed_path);
        if (!seed_file)
            throw std::runtime_error("cannot read the seed: " + seed_path.string());
        std::ostringstream text;
        text << seed_file.rdbuf();
        seed_file.close();

        auto updated = replaced(text.str(), fx_begin_marker, fx_end_marker, fx_statement);
        updated = replaced(updated, ir_begin_marker, ir_end_marker, ir_statement);
        if (updated == text.str()) {
            std::cout << "unchanged: " << seed_path.string() << '\n';
            return 0;
        }
        std::ofstream out(seed_path);
        if (!out)
            throw std::runtime_error("cannot write the seed: " + seed_path.string());
        out << updated;
        std::cout << "wrote " << fx_rows.size() << " pair(s) and " << ir_rows.size()
                  << " pillar(s) to " << seed_path.string() << '\n';
        if (unnamed_by_notation != 0)
            std::cout << unnamed_by_notation
                      << " quote(s) have no id: their provider's notation does not write them\n";
        if (unnamed_by_provider != 0)
            std::cout << unnamed_by_provider
                      << " quote(s) have no id: their provider has no notation\n";
        return 0;
    } catch (const std::exception& e) {
        std::cerr << "error: " << e.what() << '\n';
        return 1;
    }
}
