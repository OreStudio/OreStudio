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
#ifndef ORES_ORE_CORE_XML_RUN_PACKAGE_HPP
#define ORES_ORE_CORE_XML_RUN_PACKAGE_HPP

#include "ores.ore.core/export.hpp"
#include <map>
#include <string>
#include <string_view>

/**
 * @file run_package.hpp
 * @brief Where the files of an ORE input directory sit in the directory the
 * engine reads.
 *
 * The engine is started on the run document and reads every file the document
 * names from the run's @c inputPath, which defaults to @c Input. This is the
 * layout of that directory, and it is a fact about an ORE package: it reads the
 * document and the names, and holds no database.
 */
namespace ores::ore::xml {

/**
 * @brief The files of an ORE input directory, by the name the run document
 * gives each. The run document itself is @c ore.xml.
 */
using input_files = std::map<std::string, std::string>;

/**
 * @brief The name of the run document in an ORE input directory.
 */
inline constexpr std::string_view run_document_file = "ore.xml";

/**
 * @brief Where each exported file sits in the engine's working directory.
 *
 * The run document goes under @c Input, because the engine is started on it,
 * and every other file goes under the @c inputPath the document names.
 *
 * Throws when the run document's input path, or a file name, would write
 * outside the package, because both are a tenant's to edit.
 */
ORES_ORE_CORE_EXPORT input_files archive_layout(const input_files& files);

/**
 * @brief Where the engine reads the run's own data from.
 *
 * The run document names a market data file, a fixings file and a portfolio,
 * and the engine reads each from the run's @c inputPath. An import does not
 * store them, because they are a run's data and not a definition's
 * configuration, so whoever builds the engine's working directory places them
 * here. A slot the run document leaves unset is empty.
 */
struct run_data_files {
    /** The path the engine reads the market data from. */
    std::string market_data;
    /** The path the engine reads the fixings from. */
    std::string fixings;
    /** The path the engine reads the portfolio from. */
    std::string portfolio;
};

/**
 * @brief The paths the run document names for the run's own data.
 *
 * Throws when a name would write outside the package, because the run document
 * is a tenant's to edit.
 */
ORES_ORE_CORE_EXPORT run_data_files declared_data_files(const input_files& files);

}

#endif
