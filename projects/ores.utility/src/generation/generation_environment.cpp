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
#include "ores.utility/generation/generation_environment.hpp"

namespace ores::utility::generation {

generation_environment::generation_environment(entries initial)
    : entries_(std::move(initial)) {}

generation_environment::generation_environment(
    std::shared_ptr<const generation_environment> parent,
    entries overrides)
    : parent_(std::move(parent)),
      entries_(std::move(overrides)) {}

std::optional<std::string>
generation_environment::get(std::string_view key) const {
    const std::string k(key);
    if (auto it = entries_.find(k); it != entries_.end()) {
        return it->second;
    }
    if (parent_) {
        return parent_->get(key);
    }
    return std::nullopt;
}

std::string generation_environment::get_or(std::string_view key,
    const std::string& default_value) const {
    auto v = get(key);
    return v.value_or(default_value);
}

bool generation_environment::has(std::string_view key) const {
    return get(key).has_value();
}

}
