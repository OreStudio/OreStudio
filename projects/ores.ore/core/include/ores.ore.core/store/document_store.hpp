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
#ifndef ORES_ORE_CORE_STORE_DOCUMENT_STORE_HPP
#define ORES_ORE_CORE_STORE_DOCUMENT_STORE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.ore.core/domain/conventions_mapper.hpp"
#include "ores.ore.core/domain/curve_configuration_mapper.hpp"
#include "ores.ore.core/domain/pricing_engine_mapper.hpp"
#include "ores.ore.core/domain/todays_market_mapper.hpp"
#include "ores.ore.core/export.hpp"
#include <boost/uuid/uuid.hpp>
#include <string>
#include <vector>

/**
 * @file document_store.hpp
 * @brief Writes each mapped configuration document and reads it back.
 *
 * One write and one read per document kind. A write stamps the session's party
 * on every row when the session has one, so the caller owns only the content.
 * A read selects the rows of one document under the session, so row level
 * security confines it to what the session's party may see.
 */
namespace ores::ore::store {

ORES_ORE_CORE_EXPORT void write(const database::context& ctx, domain::mapped_pricing_engines v);
ORES_ORE_CORE_EXPORT domain::mapped_pricing_engines
read_pricing_engines(const database::context& ctx, const boost::uuids::uuid& config_id);

ORES_ORE_CORE_EXPORT void write(const database::context& ctx, domain::mapped_todays_market v);
ORES_ORE_CORE_EXPORT domain::mapped_todays_market
read_todays_market(const database::context& ctx, const boost::uuids::uuid& config_id);

ORES_ORE_CORE_EXPORT void write(const database::context& ctx, domain::mapped_curve_configuration v);
ORES_ORE_CORE_EXPORT domain::mapped_curve_configuration
read_curve_configuration(const database::context& ctx, const boost::uuids::uuid& config_id);

/**
 * @brief What a conventions write did not store, and why.
 */
struct conventions_write_result {
    /**
     * World conventions the tenant already held. An import never changes world
     * data, so the tenant's row stands.
     */
    std::vector<std::string> world_kept;
    /**
     * FX conventions, which are world data with no store of their own yet.
     */
    std::vector<std::string> fx_skipped;
};

/**
 * @brief Writes a conventions document.
 *
 * The instrument conventions belong to the party and replace any the party
 * holds under the same id. The index conventions are world data: one the
 * tenant lacks is added, and one it holds is left as it is.
 */
ORES_ORE_CORE_EXPORT conventions_write_result write(const database::context& ctx,
                                                    domain::mapped_conventions v);

/**
 * @brief Reads every convention the session sees: the party's instrument
 * conventions and the tenant's index conventions.
 */
ORES_ORE_CORE_EXPORT domain::mapped_conventions read_conventions(const database::context& ctx);

}

#endif
