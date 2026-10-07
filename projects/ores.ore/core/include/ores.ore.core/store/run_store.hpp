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
#ifndef ORES_ORE_CORE_STORE_RUN_STORE_HPP
#define ORES_ORE_CORE_STORE_RUN_STORE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.ore.core/export.hpp"
#include <boost/uuid/uuid.hpp>
#include <map>
#include <string>
#include <vector>

/**
 * @file run_store.hpp
 * @brief A report definition's ORE input, into the database and back out.
 *
 * The run document is stored against the report definition. Each
 * configuration document it names is stored, given a configuration row, and
 * bound to the definition in the slot its configuration type names. The export
 * walks the same chain backwards and writes the files under the names the run
 * document gives them.
 */
namespace ores::ore::store {

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
 * @brief What an import stored and what it left out.
 */
struct run_import_result {
    /** The files stored, run document first. */
    std::vector<std::string> stored;
    /**
     * Input files that hold no configuration the store keeps, such as the
     * portfolio, the market data and the fixings.
     */
    std::vector<std::string> not_stored;
    /**
     * World conventions the tenant already held. An import never changes world
     * data, so the tenant's row stands.
     */
    std::vector<std::string> world_conventions_kept;
    /** FX conventions, by ORE id, which have no store yet. */
    std::vector<std::string> fx_conventions_skipped;
};

/**
 * @brief Stores a run's configuration against a report definition.
 *
 * @param name prefixes the name of each configuration the import creates, as
 * @c name/file, so two imports under one party stay apart.
 *
 * Throws, before writing anything, when the definition already holds a run
 * document, when the run document names a configuration file the input does not
 * hold, or when it uses a parameter no definition describes. The writes are not
 * one transaction, so a failure while storing a document leaves what was
 * written before it; the error names the file.
 */
ORES_ORE_CORE_EXPORT run_import_result import_run(const database::context& ctx,
                                                  const boost::uuids::uuid& report_definition_id,
                                                  const std::string& name,
                                                  const input_files& files);

/**
 * @brief Rebuilds a report definition's ORE input: the run document and every
 * configuration document the definition binds.
 *
 * A session with no party, such as a workflow step's, sees every party's rows,
 * so the export narrows itself to the party that owns the definition.
 */
ORES_ORE_CORE_EXPORT input_files export_run(const database::context& ctx,
                                            const boost::uuids::uuid& report_definition_id);

/**
 * @brief Where each exported file sits in the engine's working directory.
 *
 * The engine is started on @c Input/ore.xml, and reads every file the run
 * document names from the run's @c inputPath, which defaults to @c Input.
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
