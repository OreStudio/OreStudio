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
#include "ores.ore.core/xml/document_kind.hpp"
#include <stdexcept>
#include <cctype>
#include <fstream>
#include <string>

namespace ores::ore::xml {

namespace {

/// How much of a document is read to find its root element.
constexpr std::size_t kPeek = 4096;

std::string read_header(const std::filesystem::path& file) {
    std::ifstream ifs(file, std::ios::binary);
    if (!ifs)
        return {};
    std::string buf(kPeek, '\0');
    ifs.read(buf.data(), static_cast<std::streamsize>(kPeek));
    buf.resize(static_cast<std::size_t>(ifs.gcount()));
    return buf;
}

/**
 * @brief Reads the name of a document's root element.
 *
 * Leading comments, processing instructions and doctype declarations are
 * skipped, so the first start tag is the root however the file is
 * decorated.
 */
std::string root_element_of(const std::filesystem::path& file) {
    const auto header = read_header(file);
    auto at = header.find('<');
    while (at != std::string::npos) {
        if (header.compare(at, 4, "<!--") == 0) {
            const auto end = header.find("-->", at + 4);
            if (end == std::string::npos)
                return {};
            at = header.find('<', end + 3);
            continue;
        }
        if (header.compare(at, 2, "<?") == 0 || header.compare(at, 2, "<!") == 0) {
            const auto end = header.find('>', at + 2);
            if (end == std::string::npos)
                return {};
            at = header.find('<', end + 1);
            continue;
        }
        auto end = at + 1;
        while (end < header.size() && (std::isalpha(static_cast<unsigned char>(header[end])) ||
                                       header[end] == '_' || header[end] == ':'))
            ++end;
        return header.substr(at + 1, end - at - 1);
    }
    return {};
}

} // namespace

const std::array<document_family, 13>& document_families() {
    static const std::array<document_family, 13> families{{
        {document_kind::portfolio, "Portfolio", true, true, "portfolio import and export"},
        {document_kind::currency_config, "CurrencyConfig", true, true, "the round trip"},
        {document_kind::calendar_adjustments, "CalendarAdjustments", true, true, "the round trip"},
        {document_kind::conventions, "Conventions", true, true,
         "run configuration import and export"},
        {document_kind::curve_configuration, "CurveConfiguration", true, true,
         "run configuration import and export"},
        {document_kind::pricing_engines, "PricingEngines", true, true,
         "run configuration import and export, and the round trip"},
        {document_kind::todays_market, "TodaysMarket", true, true,
         "run configuration import and export"},
        {document_kind::credit_simulation, "CreditSimulation", true, true, "the round trip"},
        {document_kind::counterparty_information, "CounterpartyInformation", false, false, {}},
        {document_kind::ibor_fallback_config, "IborFallbackConfig", false, false, {}},
        {document_kind::netting_set_definitions, "NettingSetDefinitions", false, false, {}},
        {document_kind::stress_testing, "StressTesting", false, false, {}},
        {document_kind::run_document, "ORE", true, true, "the run document projection"},
    }};
    return families;
}

const document_family& family_of(document_kind kind) {
    for (const auto& family : document_families())
        if (family.kind == kind)
            return family;
    // The table holds every value of the enum, which a test states.
    throw std::logic_error("a document kind with no family in the table");
}

std::optional<document_kind> detect_document_kind(const std::filesystem::path& path) {
    const auto root = root_element_of(path);
    for (const auto& family : document_families())
        if (family.root_element == root)
            return family.kind;
    return std::nullopt;
}

auto read_root_element(const std::filesystem::path& path) -> std::string {
    return root_element_of(path);
}

}
