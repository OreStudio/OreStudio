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
#include "ores.service/messaging/step_progress_board.hpp"
#include <utility>

namespace ores::service::messaging {

step_progress_board& step_progress_board::instance() {
    static step_progress_board board;
    return board;
}

void step_progress_board::report(const std::string& step_id, reported_progress progress) {
    const std::lock_guard lock(mutex_);
    by_step_.insert_or_assign(step_id, std::move(progress));
}

std::optional<reported_progress> step_progress_board::latest(const std::string& step_id) const {
    const std::lock_guard lock(mutex_);
    const auto it = by_step_.find(step_id);
    if (it == by_step_.end())
        return std::nullopt;
    return it->second;
}

void step_progress_board::forget(const std::string& step_id) {
    const std::lock_guard lock(mutex_);
    by_step_.erase(step_id);
}

}
