/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_DATABASE_REPOSITORY_HELPERS_HPP
#define ORES_DATABASE_REPOSITORY_HELPERS_HPP

#include "ores.database/repository/db_types.hpp"
#include "ores.database/repository/repository_exception.hpp"
#include "ores.logging/make_logger.hpp"
#include <boost/exception/diagnostic_information.hpp>
#include <format>
#include <string>

namespace ores::database::repository {

/**
 * @brief The bitemporal maximum timestamp, as the text a hand-written query
 * states.
 *
 * A row whose =valid_to= holds this instant is the row still current. In the
 * binary format the sentinel is =db_timestamp::max()=, which the layer's codecs
 * read and write as this instant, so generated code names the maximum and never
 * the text. This constant is what SQL that an entity model does not generate
 * carries, where a value is compared as text.
 *
 * The explicit UTC offset matches =ores_utility_infinity_timestamp_fn()= and
 * keeps the comparison independent of the session timezone.
 */
inline constexpr const char* MAX_TIMESTAMP = "9999-12-31 23:59:59+00";

/**
 * @brief Ensures a repository operation result is successful, throwing an exception if not.
 *
 * This helper checks if a repository operation returned a successful result.
 * If the result indicates an error, it logs the error and throws a repository_exception
 * with details about the failure.
 *
 * @tparam T The type of the result (must support operator bool and have an error() method)
 * @param result The result to check
 * @throws repository_exception if the result indicates failure
 *
 * @example
 * auto result = session.and_then(query);
 * ensure_success(result); // Throws if query failed
 */
template <typename T>
void ensure_success(const T& result, logging::logger_t& lg) {
    using namespace ores::logging;

    if (!result) {
        BOOST_LOG_SEV(lg, error) << result.error().what();
        BOOST_THROW_EXCEPTION(
            repository_exception(std::format("Repository error: {}", result.error().what())));
    }
}

}

#endif
