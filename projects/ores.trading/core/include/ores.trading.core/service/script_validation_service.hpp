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
#ifndef ORES_TRADING_CORE_SERVICE_SCRIPT_VALIDATION_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_SCRIPT_VALIDATION_SERVICE_HPP

#include "ores.database/domain/context_fwd.hpp"
#include "ores.trading.api/messaging/script_library_operation_protocol.hpp"
#include "ores.trading.core/export.hpp"
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <span>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief The code of one variant of a script, by its purpose.
 */
struct script_text final {
    std::string purpose;
    std::string code;
};

/**
 * @brief Validates a script's variants against the variables a trade states.
 *
 * Each variant is parsed with ORE's grammar; a variant that does not parse is
 * reported where ORE stopped, and nothing is checked further. Otherwise the
 * variants' inputs are merged, and the trade's variables are checked against
 * each variant: a finding in the text keeps its variant's purpose, and a finding
 * about a variable, which every variant would repeat, is reported once. The
 * script is valid when no finding is an error, so warnings alone do not stop a
 * booking.
 */
ORES_TRADING_CORE_EXPORT messaging::validate_script_response
validate_script(std::span<const script_text> variants,
                const std::vector<messaging::script_variable_value>& variables);

/**
 * @brief A library script as the caller's tenant resolves its name and version:
 * the tenant that holds it, the version, and its variants.
 */
struct resolved_script final {
    boost::uuids::uuid tenant_id;
    int version = 0;
    std::vector<script_text> variants;
};

/**
 * @brief A library script as the caller's tenant resolves its name: the
 * tenant's own script where it has one, else the world library's.
 *
 * @param version the script's version, or 0 for its current one.
 * @return the script, or nothing when no script of that name and version is
 * held.
 */
ORES_TRADING_CORE_EXPORT std::optional<resolved_script>
resolve_library_script(const ores::database::context& ctx, const std::string& name, int version);

/**
 * @brief The variants of a library script as the caller's tenant resolves its
 * name: the tenant's own script where it has one, else the world library's.
 *
 * @param version the script's version, or 0 for its current one.
 * @return the variants, or nothing when no script of that name and version is
 * held.
 */
ORES_TRADING_CORE_EXPORT std::optional<std::vector<script_text>>
library_variants(const ores::database::context& ctx, const std::string& name, int version);

}

#endif
