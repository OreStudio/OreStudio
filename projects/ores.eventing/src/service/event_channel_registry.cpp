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
#include "ores.eventing/service/event_channel_registry.hpp"

#include <algorithm>

namespace ores::eventing::service {

void event_channel_registry::register_channel(const std::string& name,
                                              const std::string& description) {
    std::lock_guard lock(mutex_);
    channels_[name] = domain::event_channel_info{.name = name, .description = description};
}

std::vector<domain::event_channel_info> event_channel_registry::get_channels() const {
    std::lock_guard lock(mutex_);

    std::vector<domain::event_channel_info> result;
    result.reserve(channels_.size());

    for (const auto& [name, info] : channels_) {
        result.push_back(info);
    }

    // Sort by name for consistent ordering
    std::ranges::sort(result, {}, &domain::event_channel_info::name);

    return result;
}

bool event_channel_registry::is_registered(const std::string& name) const {
    std::lock_guard lock(mutex_);
    return channels_.contains(name);
}

std::size_t event_channel_registry::size() const {
    std::lock_guard lock(mutex_);
    return channels_.size();
}

}
