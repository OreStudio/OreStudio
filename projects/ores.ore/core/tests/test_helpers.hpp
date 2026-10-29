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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_ORE_CORE_TESTS_TEST_HELPERS_HPP_
#define ORES_ORE_CORE_TESTS_TEST_HELPERS_HPP_

#include "ores.ore.core/xml/exporter.hpp"
#include "ores.ore.core/xml/importer.hpp"
#include "ores.testing/project_root.hpp"
#include "ores.testing/test_files.hpp"
#include "ores.trading.api/domain/instrument_batch_mapper.hpp"
#include <fstream>
#include <set>
#include <sstream>

using ores::ore::xml::exporter;
using ores::ore::xml::trade_import_item;
using ores::trading::domain::instrument_batch;

inline std::filesystem::path example_path(const std::string& filename) {
    return ores::testing::project_root::resolve("external/ore/examples/Products/Example_Trades/" +
                                                filename);
}

inline boost::uuids::uuid fixed_trade_id() {
    return boost::uuids::string_generator()("11111111-2222-3333-4444-555555555555");
}

inline std::string export_one(const trade_import_item& item) {
    auto instrument = item.instrument;
    ores::trading::domain::stamp_ids(instrument, fixed_trade_id());

    ores::trading::messaging::trade_export_item export_item;
    export_item.anchor = item.anchor;
    export_item.anchor.id = fixed_trade_id();
    export_item.ore_id = item.ore_id;
    export_item.envelope = item.envelope;

    instrument_batch b;
    ores::trading::domain::append_instrument(b, instrument);
    return exporter::export_portfolio({export_item}, b);
}

inline std::string first_differing_line(const std::string& a, const std::string& b) {
    std::istringstream sa(a), sb(b);
    std::string la, lb;
    while (std::getline(sa, la) && std::getline(sb, lb)) {
        if (la == lb)
            continue;
        std::size_t i = 0;
        while (i < la.size() && i < lb.size() && la[i] == lb[i])
            ++i;
        const auto from = i > 40 ? i - 40 : 0;
        return la.substr(0, la.find(' ')) + " differs near: first=..." + la.substr(from, 90) +
               " second=..." + lb.substr(from, 90);
    }
    return "the renderings differ in length";
}

inline const std::set<std::string>& known_row_differences() {
    static const std::set<std::string> differences = {};
    return differences;
}


inline ores::ore::domain::conventions load(const std::filesystem::path& path) {
    return ores::ore::domain::conventions_mapper::read(ores::testing::read_file(path),
                                                       path.string());
}

#endif
