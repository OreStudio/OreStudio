/* -*- mode: c++; tab-width: 4; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
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
#ifndef ORES_MARKETDATA_SERVICE_APP_INSTRUMENT_NAMING_HPP
#define ORES_MARKETDATA_SERVICE_APP_INSTRUMENT_NAMING_HPP

#include "ores.marketdata.api/datum/market_datum.hpp"
#include "ores.marketdata.api/datum/schema.hpp"
#include "ores.marketdata.service/export.hpp"
#include <cstddef>
#include <string>

namespace ores::marketdata::service::app {

/**
 * @brief The text a datum holds in @p f, or empty when it holds none.
 */
ORES_MARKETDATA_SERVICE_EXPORT std::string field_text(const datum::market_datum& d,
                                                      datum::field f);

/**
 * @brief The kind an instrument type and its shape make.
 *
 * A pair has no axes; a curve is a discount or a zero series read along its
 * term; a surface has two axes; anything else with one term axis is a ladder.
 * An instrument outside those five kinds is not catalogued, and an empty answer
 * says so rather than naming a kind the vocabulary does not hold.
 */
ORES_MARKETDATA_SERVICE_EXPORT std::string kind_of(const datum::schema_row& row,
                                                   std::size_t axis_count);

/**
 * @brief The display name: the subject, the type and the identity fields that
 * tell one instrument of the kind from its peers.
 *
 * A pair is spelled base/quote. A curve or a surface that carries a curve id or
 * a surface id is named by it, because that is what a person types. Everything
 * else is the subject, the type's own spelling and every other identity field
 * that holds a value, in schema order.
 */
ORES_MARKETDATA_SERVICE_EXPORT std::string name_of(const datum::market_datum& series,
                                                   const datum::schema_row& row,
                                                   const std::string& kind,
                                                   const std::string& type_spelling);

}

#endif
