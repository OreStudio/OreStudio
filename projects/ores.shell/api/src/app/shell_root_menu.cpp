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
#include "ores.shell/app/shell_root_menu.hpp"
#include <stdexcept>
#include <utility>

namespace ores::shell::app {

shell_root_menu::shell_root_menu() : cli::Menu("ores-shell") {}

void shell_root_menu::take(const std::string& name) {
    if (!names_.insert(name).second)
        throw std::runtime_error(
            "Two shell command units register the name '" + name +
            "'. One name has one owner: the first unit answers every verb the two share, "
            "and the help describes only that unit, so the second unit's verbs answer "
            "without appearing in the help. Move the second unit's verbs onto the first "
            "unit's menu as an extension.");
}

void shell_root_menu::claim(cli::Menu& menu) {
    // A menu's name is the address the CLI resolves, and cli::Command keeps it
    // protected. Menu::Prompt() is the same string for every menu the shell
    // builds, because each is constructed from its name alone; the menu-name
    // gate rejects a construction that passes a separate prompt, so the two
    // cannot drift.
    const std::string name(menu.Prompt());
    take(name);
    menus_[name] = &menu;
}

void shell_root_menu::claim(const std::string& name) {
    take(name);
}

void shell_root_menu::add_extension(const std::string& name,
                                    std::function<void(cli::Menu&)> extend) {
    extensions_[name].push_back(std::move(extend));
}

void shell_root_menu::apply_extensions() {
    for (auto& [name, extensions] : extensions_) {
        const auto found = menus_.find(name);
        if (found == menus_.end() || found->second == nullptr)
            throw std::runtime_error("A shell unit extends the name '" + name +
                                     "', which no unit registers as a menu.");
        for (auto& extend : extensions)
            extend(*found->second);
    }
}

void insert_menu(cli::Menu& root, std::unique_ptr<cli::Menu>&& menu) {
    if (auto* const owned = dynamic_cast<shell_root_menu*>(&root))
        owned->claim(*menu);
    root.Insert(std::move(menu));
}

void claim_name(cli::Menu& root, const std::string& name) {
    if (auto* const owned = dynamic_cast<shell_root_menu*>(&root))
        owned->claim(name);
}

void extend_menu(cli::Menu& root, const std::string& name,
                 std::function<void(cli::Menu&)> extend) {
    if (auto* const owned = dynamic_cast<shell_root_menu*>(&root)) {
        owned->add_extension(name, std::move(extend));
        return;
    }
    auto menu = std::make_unique<cli::Menu>(name);
    extend(*menu);
    root.Insert(std::move(menu));
}

}
