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
#ifndef ORES_COMMS_SHELL_APP_PAGINATION_CONTEXT_HPP
#define ORES_COMMS_SHELL_APP_PAGINATION_CONTEXT_HPP

#include <string>
#include <cstdint>
#include <functional>
#include <unordered_map>

namespace ores::comms::shell::app {

/**
 * @brief State for a single entity's pagination.
 */
struct entity_page_state {
    /// Current offset in the result set
    std::uint32_t current_offset = 0;
    /// Total count of available records (from last response)
    std::uint32_t total_count = 0;
};

/**
 * @brief Callback type for re-invoking list commands during navigation.
 */
using list_callback = std::function<void(std::ostream&)>;

/**
 * @brief Manages pagination state across shell commands.
 *
 * Tracks per-entity pagination state (offset, total count) and maintains
 * a registry of callbacks to re-invoke list commands during navigation.
 */
class pagination_context {
public:
    pagination_context() = default;

    /**
     * @brief Get the current page size.
     * @return The number of records per page.
     */
    std::uint32_t page_size() const;

    /**
     * @brief Set the page size.
     * @param size The number of records per page.
     */
    void set_page_size(std::uint32_t size);

    /**
     * @brief Get or create pagination state for an entity.
     * @param entity_name The name of the entity (e.g., "currencies").
     * @return Reference to the entity's pagination state.
     */
    entity_page_state& state_for(const std::string& entity_name);

    /**
     * @brief Get the last entity that was listed.
     * @return The name of the last listed entity.
     */
    const std::string& last_entity() const;

    /**
     * @brief Set the last entity that was listed.
     * @param name The name of the entity.
     */
    void set_last_entity(const std::string& name);

    /**
     * @brief Register a callback for listing an entity.
     * @param entity_name The name of the entity.
     * @param callback The callback to invoke for listing.
     */
    void register_list_callback(const std::string& entity_name,
                                list_callback callback);

    /**
     * @brief Get the list callback for an entity.
     * @param entity_name The name of the entity.
     * @return Pointer to the callback, or nullptr if not registered.
     */
    const list_callback* get_list_callback(const std::string& entity_name) const;

private:
    std::uint32_t page_size_ = 20;
    std::string last_entity_;
    std::unordered_map<std::string, entity_page_state> entity_states_;
    std::unordered_map<std::string, list_callback> list_callbacks_;
};

}

#endif
