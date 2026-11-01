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
#ifndef ORES_LOGGING_LOGGING_OPTIONS_HPP
#define ORES_LOGGING_LOGGING_OPTIONS_HPP

#include "ores.logging/export.hpp"
#include "ores.utility/codec/member_count.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include <filesystem>
#include <iosfwd>
#include <optional>
#include <string>
#include <tuple>

namespace ores::logging {

/**
 * @brief Options related to logging.
 */
struct logging_options final {
    /**
     * @brief Level at which to log.
     */
    std::string severity;
    /**
     * @brief Name of the file to log into. If empty, file logging is disabled.
     */
    std::string filename;
    /**
     * @brief If true, dumps the log into the console.
     */
    bool output_to_console = false;
    /**
     * @brief Directory in which to place the output.
     */
    std::filesystem::path output_directory;
    /**
     * @brief If true, includes the process ID in the log filename.
     *
     * When enabled, the filename is modified to include the PID before the
     * extension. For example: "app.log" becomes "app.12345.log".
     */
    bool include_pid = false;
    /**
     * @brief If set, embeds the replica index in the log filename before the
     * extension (and before the PID if include_pid is also set).
     *
     * For example: "app.log" with replica_index=0 becomes "app.0.log".
     */
    std::optional<int> replica_index;
    /**
     * @brief Tag to filter the logging. If supplied, only messages with this
     * tag will be logged.
     */
    std::string tag;
};

[[nodiscard]] constexpr auto wire_fields(const logging_options&) {
    return std::tuple{
        ::ores::utility::codec::field{"severity", &logging_options::severity},
        ::ores::utility::codec::field{"filename", &logging_options::filename},
        ::ores::utility::codec::field{"output_to_console", &logging_options::output_to_console},
        ::ores::utility::codec::field{"output_directory", &logging_options::output_directory},
        ::ores::utility::codec::field{"include_pid", &logging_options::include_pid},
        ::ores::utility::codec::field{"replica_index", &logging_options::replica_index},
        ::ores::utility::codec::field{"tag", &logging_options::tag},
    };
}

static_assert(::ores::utility::codec::names_every_member<logging_options>(),
              "logging_options has a member its wire shape does not name");

ORES_LOGGING_EXPORT std::ostream& operator<<(std::ostream& s, const logging_options& v);

}

#endif
