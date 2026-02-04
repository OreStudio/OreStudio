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
#include "ores.comms.shell/app/commands/navigation_commands.hpp"

#include <ostream>
#include <functional>
#include <cli/cli.h>

namespace ores::comms::shell::app::commands {

using namespace logging;

void navigation_commands::
register_commands(cli::Menu& root_menu, pagination_context& pagination) {
    root_menu.Insert("next", [&pagination](std::ostream& out) {
        process_next(std::ref(out), std::ref(pagination));
    }, "Show next page of the last listed entity");

    root_menu.Insert("prev", [&pagination](std::ostream& out) {
        process_prev(std::ref(out), std::ref(pagination));
    }, "Show previous page of the last listed entity");

    root_menu.Insert("first", [&pagination](std::ostream& out) {
        process_first(std::ref(out), std::ref(pagination));
    }, "Jump to first page of the last listed entity");

    root_menu.Insert("last", [&pagination](std::ostream& out) {
        process_last(std::ref(out), std::ref(pagination));
    }, "Jump to last page of the last listed entity");

    root_menu.Insert("page-size", [&pagination](std::ostream& out) {
        process_page_size(std::ref(out), std::ref(pagination), 0);
    }, "Show current page size");

    root_menu.Insert("page-size", [&pagination](std::ostream& out, int size) {
        process_page_size(std::ref(out), std::ref(pagination),
                          static_cast<std::uint32_t>(size));
    }, "Set page size (e.g., 'page-size 10')");
}

void navigation_commands::
process_next(std::ostream& out, pagination_context& pagination) {
    const auto& entity = pagination.last_entity();
    if (entity.empty()) {
        out << "No previous list command to paginate." << std::endl;
        return;
    }

    auto& state = pagination.state_for(entity);
    if (state.current_offset + pagination.page_size() >= state.total_count) {
        out << "Already on last page." << std::endl;
        return;
    }

    state.current_offset += pagination.page_size();
    BOOST_LOG_SEV(lg(), debug) << "Navigating to next page for " << entity
                               << ", offset=" << state.current_offset;

    const auto* callback = pagination.get_list_callback(entity);
    if (callback) {
        (*callback)(out);
    } else {
        out << "No list callback registered for: " << entity << std::endl;
    }
}

void navigation_commands::
process_prev(std::ostream& out, pagination_context& pagination) {
    const auto& entity = pagination.last_entity();
    if (entity.empty()) {
        out << "No previous list command to paginate." << std::endl;
        return;
    }

    auto& state = pagination.state_for(entity);
    if (state.current_offset == 0) {
        out << "Already on first page." << std::endl;
        return;
    }

    if (state.current_offset < pagination.page_size()) {
        state.current_offset = 0;
    } else {
        state.current_offset -= pagination.page_size();
    }

    BOOST_LOG_SEV(lg(), debug) << "Navigating to previous page for " << entity
                               << ", offset=" << state.current_offset;

    const auto* callback = pagination.get_list_callback(entity);
    if (callback) {
        (*callback)(out);
    } else {
        out << "No list callback registered for: " << entity << std::endl;
    }
}

void navigation_commands::
process_first(std::ostream& out, pagination_context& pagination) {
    const auto& entity = pagination.last_entity();
    if (entity.empty()) {
        out << "No previous list command to paginate." << std::endl;
        return;
    }

    auto& state = pagination.state_for(entity);
    if (state.current_offset == 0) {
        out << "Already on first page." << std::endl;
        return;
    }

    state.current_offset = 0;
    BOOST_LOG_SEV(lg(), debug) << "Navigating to first page for " << entity;

    const auto* callback = pagination.get_list_callback(entity);
    if (callback) {
        (*callback)(out);
    } else {
        out << "No list callback registered for: " << entity << std::endl;
    }
}

void navigation_commands::
process_last(std::ostream& out, pagination_context& pagination) {
    const auto& entity = pagination.last_entity();
    if (entity.empty()) {
        out << "No previous list command to paginate." << std::endl;
        return;
    }

    auto& state = pagination.state_for(entity);
    if (state.total_count == 0) {
        out << "No records available." << std::endl;
        return;
    }

    // Calculate offset for the last page
    const auto page_size = pagination.page_size();
    std::uint32_t last_page_offset = 0;
    if (state.total_count > page_size) {
        last_page_offset = ((state.total_count - 1) / page_size) * page_size;
    }

    if (state.current_offset == last_page_offset) {
        out << "Already on last page." << std::endl;
        return;
    }

    state.current_offset = last_page_offset;
    BOOST_LOG_SEV(lg(), debug) << "Navigating to last page for " << entity
                               << ", offset=" << state.current_offset;

    const auto* callback = pagination.get_list_callback(entity);
    if (callback) {
        (*callback)(out);
    } else {
        out << "No list callback registered for: " << entity << std::endl;
    }
}

void navigation_commands::
process_page_size(std::ostream& out, pagination_context& pagination,
                  std::uint32_t size) {
    if (size == 0) {
        out << "Current page size: " << pagination.page_size() << std::endl;
        return;
    }

    pagination.set_page_size(size);
    BOOST_LOG_SEV(lg(), info) << "Page size set to " << size;
    out << "Page size set to " << size << std::endl;
}

}
