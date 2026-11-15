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
#ifndef ORES_MARKETDATA_OFFICIAL_DOMAIN_SECRET_HPP
#define ORES_MARKETDATA_OFFICIAL_DOMAIN_SECRET_HPP

#include "ores.marketdata.official/export.hpp"
#include <expected>
#include <functional>
#include <optional>
#include <string>
#include <string_view>

namespace ores::marketdata::official::domain {

/**
 * @brief Reads one variable of the process environment, or gives nothing when it is not set.
 *
 * "Not set" is @c std::nullopt. A lookup that answers a null @c const @c char* instead is
 * building a @c std::string from a null pointer, which throws rather than reading as unset, so
 * a stand-in for a missing variable returns the optional and never a pointer.
 */
using environment_lookup = std::function<std::optional<std::string>(const char*)>;

/**
 * @brief The secret that @p ref names, such as the API key of a publisher.
 *
 * A ref is =env:NAME=, which names a variable of the process environment, such as
 * =env:ORES_MARKETDATA_BANXICO_API_KEY=. The services read their variables from the
 * environment's =.env=, which git ignores. A provider's =credential_ref= holds the same ref,
 * never the secret.
 *
 * An unknown form of ref, or a variable that is not set or is empty, is an error. The error
 * names the variable and never holds a value.
 */
[[nodiscard]] ORES_MARKETDATA_OFFICIAL_EXPORT std::expected<std::string, std::string>
resolve_secret(std::string_view ref, const environment_lookup& lookup);

/// The secret that @p ref names, read from the environment of this process.
[[nodiscard]] ORES_MARKETDATA_OFFICIAL_EXPORT std::expected<std::string, std::string>
resolve_secret(std::string_view ref);

}

#endif
