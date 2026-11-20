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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_LOGGING_LOGGER_HPP
#define ORES_LOGGING_LOGGER_HPP

#include "ores.logging/export.hpp"
#include "ores.logging/severity_level.hpp"
#include <memory>
#include <sstream>
#include <string_view>
#include <utility>

namespace ores::logging {

/**
 * @brief The severity of a record, spelled without the enum's name.
 *
 * A call site writes =ORES_LOG_SEV(lg(), debug)=, and =debug= here is the same
 * value as =severity_level::debug=. They are constants rather than enumerators
 * because =severity_level= is scoped, and a call site reads better without the
 * qualification.
 */
inline constexpr severity_level trace = severity_level::trace;
inline constexpr severity_level debug = severity_level::debug;
inline constexpr severity_level info = severity_level::info;
inline constexpr severity_level warn = severity_level::warn;
inline constexpr severity_level error = severity_level::error;
inline constexpr severity_level fatal = severity_level::fatal;

namespace detail {
struct logger_core;
}

/**
 * @brief A handle to one component's logger.
 *
 * The backend is Boost.Log, and it is not named here: this header is included
 * by the source of every component, and the backend's own headers cost several
 * seconds to parse. Copying a handle copies a pointer.
 */
class ORES_LOGGING_EXPORT logger final {
public:
    logger();

    explicit logger(std::shared_ptr<detail::logger_core> core) noexcept
        : core_(std::move(core)) {}

    [[nodiscard]] const std::shared_ptr<detail::logger_core>& core() const noexcept {
        return core_;
    }

private:
    std::shared_ptr<detail::logger_core> core_;
};

using logger_t = logger;

/**
 * @brief The logger of one component, named by its channel.
 */
ORES_LOGGING_EXPORT logger_t make_logger(std::string_view component_name);

/**
 * @brief A named attribute that lasts as long as the object.
 *
 * The sinks read attributes such as =Tag= to tell a test run's records from a
 * service's, so a scope that wants one adds it here and drops it when it ends.
 */
class ORES_LOGGING_EXPORT scoped_tag final {
public:
    scoped_tag(logger_t& logger, std::string_view name, std::string_view value);

    ~scoped_tag();

    scoped_tag(const scoped_tag&) = delete;
    scoped_tag& operator=(const scoped_tag&) = delete;

private:
    struct state;

    std::shared_ptr<state> state_;
};

/**
 * @brief One record, streamed into and written when it goes out of scope.
 *
 * The record is formatted whether or not the sink keeps it, which is the price
 * of not naming the backend's record type here.
 */
class ORES_LOGGING_EXPORT log_record final {
public:
    log_record(logger_t& logger, severity_level severity);

    ~log_record();

    log_record(const log_record&) = delete;
    log_record& operator=(const log_record&) = delete;

    template <typename T>
    log_record& operator<<(const T& value) {
        text_ << value;
        return *this;
    }

private:
    logger_t logger_;
    severity_level severity_;
    std::ostringstream text_;
};

}

/**
 * @brief Adds @p name with @p value to @p logger for the enclosing scope.
 */
#define ORES_LOG_SCOPED_TAG(logger, name, value) \
    ::ores::logging::scoped_tag ores_log_scoped_tag_##__LINE__(logger, name, value)

/**
 * @brief Writes one record at @p severity through @p logger.
 *
 * The severity is one of the constants beside the logger: =trace=, =debug=,
 * =info=, =warn=, =error= or =fatal=.
 */
#define ORES_LOG_SEV(logger, severity) ::ores::logging::log_record(logger, severity)

#endif
