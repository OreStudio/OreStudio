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
#ifndef ORES_ORE_CORE_XML_DOCUMENT_KIND_HPP
#define ORES_ORE_CORE_XML_DOCUMENT_KIND_HPP

#include "ores.ore.core/export.hpp"
#include <array>
#include <filesystem>
#include <optional>
#include <string_view>

namespace ores::ore::xml {

/**
 * @brief The ORE document kinds the XML facet reads and writes.
 *
 * One value for each family the generator emits, and no other: a family without
 * a value cannot be classified, and the coverage table says what reaches it.
 */
enum class document_kind {
    portfolio,
    currency_config,
    calendar_adjustments,
    conventions,
    curve_configuration,
    pricing_engines,
    todays_market,
    credit_simulation,
    counterparty_information,
    ibor_fallback_config,
    netting_set_definitions,
    stress_testing,
    run_document,
};

/**
 * @brief One ORE document family, and what the tree does with it.
 *
 * The table this describes is the one place a family is classified: the root
 * element that names it, whether the codec reads it, whether it writes it, and
 * the surface that reaches it. A family the generator emits is in the table
 * whether or not anything converts it, so an unsupported family says so rather
 * than being absent.
 */
struct document_family {
    /// The kind the root element maps to.
    document_kind kind;
    /// The root element ORE writes for the family.
    std::string_view root_element;
    /// Whether the codec reads the family.
    bool read;
    /// Whether the codec writes the family.
    bool write;
    /// The operation that reaches the family, or empty when none does.
    std::string_view surface;
};

/**
 * @brief Every family the generator emits, in one table.
 */
ORES_ORE_CORE_EXPORT const std::array<document_family, 13>& document_families();

/**
 * @brief The family of a kind, which is in the table.
 */
ORES_ORE_CORE_EXPORT const document_family& family_of(document_kind kind);

/**
 * @brief Names the document kind a file holds, from its root element.
 *
 * The root element alone decides. The rule is shared: the round trip and
 * the import directory scanner both ask this function, so a file cannot be
 * one kind to one caller and another kind to the other. Matching a keyword
 * in the opening bytes instead would both miss a root element spelled with
 * attributes and match a keyword inside a comment or a later element.
 *
 * @param path Path to the file
 * @return The kind, or @c std::nullopt when the root element is not one of
 * them, or the file cannot be read
 */
ORES_ORE_CORE_EXPORT std::optional<document_kind>
detect_document_kind(const std::filesystem::path& path);

/**
 * @brief The name of a document's root element, or empty when none is read.
 *
 * The same reading decides a document's kind, so a caller that selects
 * documents by root element asks this rather than parsing a prefix of the
 * file itself: leading comments, processing instructions and a doctype are
 * skipped by one rule.
 */
ORES_ORE_CORE_EXPORT auto read_root_element(const std::filesystem::path& path) -> std::string;

}

#endif
