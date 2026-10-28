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
#ifndef ORES_ORE_CORE_XML_RUN_DOCUMENT_FILES_HPP
#define ORES_ORE_CORE_XML_RUN_DOCUMENT_FILES_HPP

#include "ores.ore.core/export.hpp"
#include <array>
#include <optional>
#include <string_view>

namespace ores::ore::xml {

/**
 * @brief One configuration document a run can name: the configuration type it
 * fills and the Setup parameter whose value is its file.
 */
struct configuration_document {
    std::string_view configuration_type;
    std::string_view setup_parameter;
};

/**
 * @brief The configuration documents an ORE run names, in the order a reader
 * stores them: a curve segment names its conventions, so the conventions come
 * first.
 *
 * The names are a fact of ORE and not of any store. @c ore.xsd types the Setup
 * parameter list without naming its entries, so the names are taken from the
 * run documents ORE ships, and the suite holds this table against that corpus.
 */
inline constexpr std::array<configuration_document, 4> configuration_documents{{
    {"conventions", "conventionsFile"},
    {"curve_configuration", "curveConfigFile"},
    {"todays_market", "marketConfigFile"},
    {"pricing_engines", "pricingEnginesFile"},
}};

/**
 * @brief The Setup parameter that names the file of a configuration type, or
 * nothing when the type is not one a run names.
 */
ORES_ORE_CORE_EXPORT std::optional<std::string_view>
setup_parameter_of(std::string_view configuration_type);

}

#endif
