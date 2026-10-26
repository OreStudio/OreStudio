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
 * @brief Writes the generated half of the market data vendor instrument seed.
 *
 * The seed installs one vendor instrument for each pillar of each interest rate curve the
 * synthetic feeds publish, in the notation of the curve's own provider, and maps it to the oresmd
 * item the pillar's quote key names. Neither half is written by hand: the external id is what the
 * provider's notation writes for the pillar, and the item is what the pillar key projects to. This
 * program is the one writer of both, so the ids in the seed and the ids the runtime writes cannot
 * drift.
 *
 * The input is one line for each pillar of each artefact IR curve config, from the populate script
 * that reads them:
 *
 *     provider_code <TAB> currency_code <TAB> index_family <TAB> instrument_code <TAB>
 *     start_tenor_code <TAB> end_tenor_code <TAB> tenor
 *
 * @c tenor is the config's index tenor field, which decides the series the pillar lands in; an
 * empty value and the meeting-dated FOMC value both name an overnight index. The output is the
 * SQL statement that fills the seed's lookup table, which the seed joins to the artefact tables at
 * populate time.
 *
 * A pillar the provider's notation cannot write -- a meeting-dated pillar is spelled with enough
 * codes no notation writes, and a provider with no notation writes nothing at all -- gets a row
 * with the four id columns null, so the seed can tell "no id for this pillar" from "this pillar is
 * new and the seed is stale".
 *
 * Usage, from the repository root -- the query is the input, written to a file by psql, so no
 * shell pipeline runs the compass command that produces it:
 *
 *     ./compass.sh sql -- -t -A -F $'\t' -o tmp/ir-pillars.tsv -c "<the query below>"
 *     build/publish/bin/ores.marketdata.ir-catalogue --write tmp/ir-pillars.tsv
 *
 * The query, which the seed's header repeats:
 *
 *     select c.provider_code, c.currency_code, c.index_family, e.instrument_code,
 *            e.start_tenor_code, e.end_tenor_code, c.tenor
 *     from ores_dq_ir_curve_generation_configs_artefact_tbl c
 *     join ores_dq_ir_curve_template_entries_artefact_tbl e
 *       on e.dataset_id = c.dataset_id
 *      and e.currency_code = c.currency_code
 *      and e.index_family = c.index_family
 *      and e.tenor = c.tenor
 *     where c.provider_code is not null
 *     order by 1, 2, 3, 4, 5, 6
 *
 * With no --write the statement goes to stdout. With no input file the pillars are read from
 * stdin.
 */
#include "ores.marketdata.core/oresmd/pillar_quote_key.hpp"
#include "ores.marketdata.core/vendor/provider_notation.hpp"
#include "ores.marketdata.core/vendor/quote_description.hpp"
#include "ores.synthetic.api/domain/ir_curve_generation_config.hpp"
#include "ores.synthetic.api/feeds/ir_curve_template_resolver.hpp"
#include <algorithm>
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
#include <vector>

namespace {

namespace vendor = ores::marketdata::vendor;
namespace core = ores::marketdata::core;

/// The line pair the program owns in the seed. Everything between them is rewritten.
constexpr std::string_view begin_marker = "-- BEGIN GENERATED ir pillar ids";
constexpr std::string_view end_marker = "-- END GENERATED ir pillar ids";

/// The seed the program writes to when it is not told otherwise.
constexpr std::string_view default_seed =
    "projects/ores.sql/populate/marketdata/marketdata_vendor_instruments_populate.sql";

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

/// One line of the seed's lookup table.
///
/// The config's own tenor is part of the row's identity rather than a column of its own: it is
/// what the seed joins on, and the index tenor the item names follows from it.
struct catalogue_row final {
    pillar_feed pillar;
    std::optional<ores::marketdata::messaging::vendor_id> id;
    std::optional<std::string> oresmd_uri;
};

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

/// The pillar description the feed offers an adapter: the template entry's own words.
vendor::ir_curve_pillar description_of(const pillar_feed& p) {
    return vendor::ir_curve_pillar{
        p.currency_code, p.index_family, p.instrument_code, p.start_tenor_code, p.end_tenor_code};
}

/// The oresmd item the pillar's own quote key names.
///
/// The template names both ends of the pillar by tenors, so the dates are not part of the key: a
/// pillar that starts at spot holds 0D and one that starts at a period holds that period. The
/// dates are passed because the key takes them, and a pillar whose tenors reach them is one no
/// notation writes, which this program never asks for a URI.
std::string datum_uri_of(const pillar_feed& p, const std::string& index_tenor) {
    const auto start_date = std::chrono::year{2026} / 1 / 1;
    const auto end_date = std::chrono::year{2026} / 1 / 2;
    const auto key = core::make_pillar_quote_key(
        p.currency_code, index_tenor, p.start_tenor_code, start_date, end_date, p.end_tenor_code);
    return core::pillar_datum_uri(key);
}

/// The row for one pillar: the provider's id and the pillar's item, or null for both when the
/// provider cannot name the pillar.
catalogue_row row_of(const pillar_feed& p,
                     const std::string& index_tenor,
                     std::size_t& unnamed_by_notation,
                     std::size_t& unnamed_by_provider) {
    catalogue_row row{.pillar = p, .id = std::nullopt, .oresmd_uri = std::nullopt};
    const auto* notation = vendor::notation_of(p.provider_code);
    if (notation == nullptr) {
        ++unnamed_by_provider;
        return row;
    }

    const auto written = notation->write(description_of(p));
    if (!written) {
        std::cerr << "no id: " << p.provider_code << " " << p.currency_code << " " << p.index_family
                  << " " << p.instrument_code << " " << p.start_tenor_code << " "
                  << p.end_tenor_code << ": " << written.error().message << '\n';
        ++unnamed_by_notation;
        return row;
    }

    // A notation writes what it reads: an id that reads back as another pillar would map the
    // curve's ticks to another curve's item.
    const auto read = notation->read(*written);
    if (!read || !(*read == vendor::quote_description{description_of(p)}))
        throw std::runtime_error("the notation of " + p.provider_code + " wrote '" +
                                 written->external_id + "' for a pillar it does not read back");

    row.id = *written;
    row.oresmd_uri = datum_uri_of(p, index_tenor);
    return row;
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

/// The statement that fills the seed's lookup table, with the counts of what each provider got.
std::string statement_of(const std::vector<catalogue_row>& rows) {
    std::map<std::string, std::pair<std::size_t, std::size_t>> by_provider;
    for (const auto& r : rows) {
        auto& counts = by_provider[r.pillar.provider_code];
        if (r.id)
            ++counts.first;
        else
            ++counts.second;
    }

    std::string out;
    for (const auto& [provider, counts] : by_provider)
        out += std::format("    -- {}: {} pillar(s) with an id, {} without\n",
                           provider,
                           counts.first,
                           counts.second);

    out += "    insert into t_ir_pillar_id (provider_code, currency_code, index_family,\n"
           "        instrument_code, start_tenor_code, end_tenor_code, config_tenor,\n"
           "        id_scheme, external_id, field, oresmd_uri) values\n";
    for (std::size_t i = 0; i < rows.size(); ++i) {
        const auto& r = rows[i];
        out += std::format("    ({}, {}, {}, {}, {}, {}, {}, ",
                           quoted(r.pillar.provider_code),
                           quoted(r.pillar.currency_code),
                           quoted(r.pillar.index_family),
                           quoted(r.pillar.instrument_code),
                           quoted(r.pillar.start_tenor_code),
                           quoted(r.pillar.end_tenor_code),
                           quoted(r.pillar.tenor));
        if (r.id) {
            out += std::format("{}, {}, {}, {}",
                               quoted(r.id->id_scheme),
                               quoted(r.id->external_id),
                               quoted(r.id->field),
                               quoted(*r.oresmd_uri));
        } else {
            out += "null, null, null, null";
        }
        out += (i + 1 == rows.size()) ? ");\n" : "),\n";
    }
    return out;
}

/// The seed's text with its generated region replaced by @p statement.
std::string replaced(const std::string& seed, const std::string& statement, bool& found) {
    const auto begin = seed.find(begin_marker);
    const auto end = seed.find(end_marker);
    if (begin == std::string::npos || end == std::string::npos || end < begin) {
        found = false;
        return seed;
    }
    found = true;
    const auto after_begin = seed.find('\n', begin);
    if (after_begin == std::string::npos)
        throw std::runtime_error("the seed's begin marker ends its file");
    return seed.substr(0, after_begin + 1) + statement + seed.substr(end);
}

/// The input, read from stdin: one line for each pillar of each artefact IR curve config.
std::vector<pillar_feed> read_pillars(std::istream& in) {
    std::vector<pillar_feed> out;
    std::string line;
    while (std::getline(in, line)) {
        if (line.empty())
            continue;
        const auto fields = split_tabs(line);
        if (fields.size() != 7)
            throw std::runtime_error("expected seven tab separated fields, got '" + line + "'");
        out.push_back(pillar_feed{
            fields[0], fields[1], fields[2], fields[3], fields[4], fields[5], fields[6]});
    }
    return out;
}

/// The identity of a row: one pillar of one curve of one provider, named the way the config that
/// quotes it names it. Two configs that differ only in the index tenor are two rows, because their
/// pillars land in two series.
auto row_key(const pillar_feed& p) {
    return std::tie(p.provider_code,
                    p.currency_code,
                    p.index_family,
                    p.instrument_code,
                    p.start_tenor_code,
                    p.end_tenor_code,
                    p.tenor);
}

/// The row order, so a rerun over the same pillars writes the same file.
void sort_rows(std::vector<catalogue_row>& rows) {
    std::ranges::sort(
        rows, [](const auto& a, const auto& b) { return row_key(a.pillar) < row_key(b.pillar); });
    rows.erase(std::ranges::unique(rows,
                                   [](const auto& a, const auto& b) {
                                       return row_key(a.pillar) == row_key(b.pillar);
                                   })
                   .begin(),
               rows.end());
}

}

int main(int argc, char** argv) {
    auto seed_path = std::filesystem::path(std::string(default_seed));
    std::optional<std::filesystem::path> pillars_path;
    bool write = false;
    for (int i = 1; i < argc; ++i) {
        const std::string arg = argv[i];
        if (arg == "--write") {
            write = true;
        } else if (arg == "--seed" && i + 1 < argc) {
            seed_path = std::filesystem::path(argv[++i]);
        } else if (arg.starts_with("--")) {
            std::cerr << "usage: " << argv[0]
                      << " [--seed FILE] [--write] [PILLARS]\n"
                         "  Reads one line for each pillar of each artefact IR curve config:\n"
                         "  provider, currency, index family, instrument, start tenor, end tenor,\n"
                         "  tenor, tab separated. Writes the seed's lookup table.\n";
            return 2;
        } else {
            pillars_path = std::filesystem::path(arg);
        }
    }

    try {
        std::ifstream file;
        std::istream* in = &std::cin;
        if (pillars_path) {
            file.open(*pillars_path);
            if (!file)
                throw std::runtime_error("cannot read the pillars: " + pillars_path->string());
            in = &file;
        }
        const auto pillars = read_pillars(*in);
        if (pillars.empty()) {
            std::cerr << "nothing to do: the input holds no pillar\n";
            return 1;
        }

        std::size_t unnamed_by_notation = 0;
        std::size_t unnamed_by_provider = 0;
        std::vector<catalogue_row> rows;
        rows.reserve(pillars.size());
        for (const auto& p : pillars) {
            ores::synthetic::domain::ir_curve_generation_config cfg;
            cfg.currency_code = p.currency_code;
            cfg.index_family = p.index_family;
            cfg.tenor = p.tenor;
            rows.push_back(row_of(p,
                                  ores::synthetic::feed::ir_curve_index_tenor(cfg),
                                  unnamed_by_notation,
                                  unnamed_by_provider));
        }
        sort_rows(rows);
        const auto statement = statement_of(rows);

        if (!write) {
            std::cout << statement;
            return 0;
        }

        std::ifstream seed_file(seed_path);
        if (!seed_file)
            throw std::runtime_error("cannot read the seed: " + seed_path.string());
        std::ostringstream text;
        text << seed_file.rdbuf();
        seed_file.close();

        bool found = false;
        const auto updated = replaced(text.str(), statement, found);
        if (!found)
            throw std::runtime_error("the seed holds no generated region: " + seed_path.string());
        if (updated == text.str()) {
            std::cout << "unchanged: " << seed_path.string() << '\n';
            return 0;
        }
        std::ofstream out(seed_path);
        if (!out)
            throw std::runtime_error("cannot write the seed: " + seed_path.string());
        out << updated;
        std::cout << "wrote " << rows.size() << " pillar(s) to " << seed_path.string() << '\n';
        if (unnamed_by_notation != 0)
            std::cout << unnamed_by_notation
                      << " pillar(s) have no id: their provider's notation does not write them\n";
        if (unnamed_by_provider != 0)
            std::cout << unnamed_by_provider
                      << " pillar(s) have no id: their provider has no notation\n";
        return 0;
    } catch (const std::exception& e) {
        std::cerr << "error: " << e.what() << '\n';
        return 1;
    }
}
