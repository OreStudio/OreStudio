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
#ifndef ORES_LOGGING_SCOPED_ATTRIBUTE_HPP
#define ORES_LOGGING_SCOPED_ATTRIBUTE_HPP

#include <string>
#include <optional>
#include <boost/log/core.hpp>
#include <boost/log/attributes/constant.hpp>
#include <boost/log/attributes/attribute_set.hpp>

namespace ores::logging {

/**
 * @brief RAII guard that adds a thread-local attribute to Boost.Log for the
 * duration of its scope.
 *
 * This class adds a named boolean attribute with value `true` to the
 * thread-local attribute set in Boost.Log's core. The attribute is automatically
 * removed when the guard goes out of scope.
 *
 * All log records created on the same thread while the guard is active will
 * have this attribute, including records from nested function calls. This makes
 * it useful for marking entire code paths that should be treated specially by
 * certain sinks (e.g., skipping telemetry sinks to prevent recursive logging).
 *
 * Example usage:
 * @code
 * void send_telemetry() {
 *     scoped_attribute guard("SkipTelemetry");
 *     // All logs in this scope (and any functions called) will have
 *     // the SkipTelemetry attribute set to true.
 *     BOOST_LOG_SEV(lg, info) << "Sending...";
 *     do_network_stuff();  // Logs here also have the attribute
 * }
 * @endcode
 *
 * @note This class is not copyable but is movable.
 */
class scoped_attribute {
public:
    using iterator = boost::log::attribute_set::iterator;

    /**
     * @brief Constructs a scoped attribute guard.
     *
     * Adds a boolean attribute with the given name and value `true` to the
     * thread-local attribute set.
     *
     * @param attribute_name The name of the attribute to add.
     */
    explicit scoped_attribute(const std::string& attribute_name) {
        auto result = boost::log::core::get()->add_thread_attribute(
            attribute_name,
            boost::log::attributes::constant<bool>(true));
        if (result.second) {
            iterator_ = result.first;
        }
    }

    ~scoped_attribute() {
        if (iterator_.has_value()) {
            boost::log::core::get()->remove_thread_attribute(*iterator_);
        }
    }

    // Non-copyable
    scoped_attribute(const scoped_attribute&) = delete;
    scoped_attribute& operator=(const scoped_attribute&) = delete;

    // Movable
    scoped_attribute(scoped_attribute&& other) noexcept
        : iterator_(std::move(other.iterator_)) {
        other.iterator_.reset();
    }

    scoped_attribute& operator=(scoped_attribute&& other) noexcept {
        if (this != &other) {
            if (iterator_.has_value()) {
                boost::log::core::get()->remove_thread_attribute(*iterator_);
            }
            iterator_ = std::move(other.iterator_);
            other.iterator_.reset();
        }
        return *this;
    }

private:
    std::optional<iterator> iterator_;
};

}

#endif
