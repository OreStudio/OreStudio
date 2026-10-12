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
#ifndef ORES_MARKETDATA_CORE_SERVICE_ORESMD_SCHEMA_DESCRIBER_HPP
#define ORES_MARKETDATA_CORE_SERVICE_ORESMD_SCHEMA_DESCRIBER_HPP

#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.core/export.hpp"

namespace ores::marketdata::service {

/**
 * @brief Describes the oresmd schema to a client.
 *
 * The answer is the codec's own tables read as data: every instrument type with
 * its fields in key order, the asset classes, the quote types and the index
 * families. It holds no copy of the grammar, so a row added to the schema reaches
 * every client without a change here.
 */
class ORES_MARKETDATA_CORE_EXPORT oresmd_schema_describer final {
public:
    /// The schema. It is the same for every caller and never fails.
    static messaging::describe_oresmd_schema_response describe();
};

}

#endif
