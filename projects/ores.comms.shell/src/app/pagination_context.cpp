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
#include "ores.comms.shell/app/pagination_context.hpp"

namespace ores::comms::shell::app {

std::uint32_t pagination_context::page_size() const {
    return page_size_;
}

void pagination_context::set_page_size(std::uint32_t size) {
    page_size_ = size > 0 ? size : 1;
}

entity_page_state& pagination_context::state_for(const std::string& entity_name) {
    return entity_states_[entity_name];
}

const std::string& pagination_context::last_entity() const {
    return last_entity_;
}

void pagination_context::set_last_entity(const std::string& name) {
    last_entity_ = name;
}

void pagination_context::register_list_callback(const std::string& entity_name,
                                                list_callback callback) {
    list_callbacks_[entity_name] = std::move(callback);
}

const list_callback*
pagination_context::get_list_callback(const std::string& entity_name) const {
    auto it = list_callbacks_.find(entity_name);
    if (it != list_callbacks_.end()) {
        return &it->second;
    }
    return nullptr;
}

}
