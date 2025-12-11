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
#ifndef ORES_UTILITY_REPOSITORY_VERSION_CONFLICT_EXCEPTION_HPP
#define ORES_UTILITY_REPOSITORY_VERSION_CONFLICT_EXCEPTION_HPP

#include <string>
#include <format>
#include <optional>
#include <boost/exception/info.hpp>

namespace ores::utility::repository {

/**
 * @brief Exception thrown when an optimistic locking version conflict occurs.
 *
 * This exception indicates that an entity update failed because the version
 * provided by the client does not match the current version in the database.
 * This typically happens when another client has modified the entity between
 * the time it was read and the time the update was attempted.
 */
class version_conflict_exception : public virtual std::exception,
                                   public virtual boost::exception {
public:
    /**
     * @brief Constructs a version conflict exception with detailed information.
     *
     * @param entity_type The type of entity that had the version conflict
     * @param entity_id The identifier of the entity
     * @param expected_version The version the client expected (sent with update)
     * @param actual_version The current version in the database
     */
    version_conflict_exception(std::string_view entity_type,
                               std::string_view entity_id,
                               int expected_version,
                               int actual_version)
        : entity_type_(entity_type),
          entity_id_(entity_id),
          expected_version_(expected_version),
          actual_version_(actual_version),
          message_(std::format(
              "Version conflict for {} '{}': expected version {}, but current version is {}",
              entity_type, entity_id, expected_version, actual_version)) {}

    /**
     * @brief Constructs a version conflict exception with a custom message.
     *
     * When using this constructor, entity_type, entity_id, expected_version,
     * and actual_version will be std::nullopt, indicating that this detailed
     * information is not available.
     */
    explicit version_conflict_exception(std::string_view message = "")
        : message_(message) {}

    [[nodiscard]] const char* what() const noexcept override {
        return message_.c_str();
    }

    [[nodiscard]] const std::optional<std::string>& entity_type() const noexcept {
        return entity_type_;
    }

    [[nodiscard]] const std::optional<std::string>& entity_id() const noexcept {
        return entity_id_;
    }

    [[nodiscard]] std::optional<int> expected_version() const noexcept {
        return expected_version_;
    }

    [[nodiscard]] std::optional<int> actual_version() const noexcept {
        return actual_version_;
    }

private:
    std::optional<std::string> entity_type_;
    std::optional<std::string> entity_id_;
    std::optional<int> expected_version_;
    std::optional<int> actual_version_;
    std::string message_;
};

}

#endif
