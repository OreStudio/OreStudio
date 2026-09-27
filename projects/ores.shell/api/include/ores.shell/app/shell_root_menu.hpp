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
#ifndef ORES_SHELL_APP_SHELL_ROOT_MENU_HPP
#define ORES_SHELL_APP_SHELL_ROOT_MENU_HPP

#include "ores.shell/export.hpp"
#include <cli/cli.h>
#include <functional>
#include <map>
#include <memory>
#include <set>
#include <string>
#include <vector>

namespace ores::shell::app {

/**
 * @brief The REPL's root menu, and the one owner of every menu name under it.
 *
 * cli::Menu holds its children in a vector and answers a name by first match, so
 * two units that register the same menu name silently shadow each other: the
 * first unit answers every verb the two share, and the help describes only that
 * unit, so the other unit's verbs are invisible although they still answer.
 * This subclass refuses a second claim on a name, which turns that class of
 * defect into a failure at startup.
 *
 * It also collects the verbs a unit contributes to a menu another unit owns. A
 * generated entity unit owns the menu named for its entity, and the
 * hand-written unit beside it contributes the verbs the model cannot express,
 * so one address holds both sets and the help lists both.
 */
class ORES_SHELL_EXPORT shell_root_menu final : public cli::Menu {
public:
    shell_root_menu();

    /**
     * @brief Take ownership of a menu name, recording the menu that answers it.
     *
     * @throws std::runtime_error when another unit already owns the name.
     */
    void claim(cli::Menu& menu);

    /**
     * @brief Take ownership of a name whose command is inserted directly.
     *
     * A unit that inserts a verb into the root rather than a submenu owns that
     * name just as a menu does, and collides with a menu of the same name in
     * just the same way.
     *
     * @throws std::runtime_error when another unit already owns the name.
     */
    void claim(const std::string& name);

    /**
     * @brief Record that the menu named @p name gains the verbs @p extend adds.
     *
     * The name need not be claimed yet: the units register in whatever order
     * repl.cpp calls them, and the two halves of one menu are two of those
     * calls.
     */
    void add_extension(const std::string& name, std::function<void(cli::Menu&)> extend);

    /**
     * @brief Run every recorded extension against the menu it names.
     *
     * Called once, after every unit has registered, so the order those units
     * registered in cannot decide whether an extension applies.
     *
     * @throws std::runtime_error when an extension names a menu no unit owns,
     * because that extension would otherwise do nothing and say nothing.
     */
    void apply_extensions();

private:
    void take(const std::string& name);

    std::set<std::string> names_;
    std::map<std::string, cli::Menu*> menus_;
    std::map<std::string, std::vector<std::function<void(cli::Menu&)>>> extensions_;
};

/**
 * @brief Insert a unit's menu into the root, claiming its name.
 *
 * Every generated shell unit calls this rather than cli::Menu::Insert, which is
 * what lets the root see the name. A root that is not a shell_root_menu -- a
 * unit test's own, say -- inserts without claiming, because it is not the
 * process's menu and it has no other unit to collide with.
 */
ORES_SHELL_EXPORT void insert_menu(cli::Menu& root, std::unique_ptr<cli::Menu>&& menu);

/**
 * @brief Claim a root name for a command the caller inserts itself.
 *
 * The counterpart of insert_menu for a verb that lives in the root menu rather
 * than in a submenu, so both kinds of child are owned the same way.
 */
ORES_SHELL_EXPORT void claim_name(cli::Menu& root, const std::string& name);

/**
 * @brief Add verbs to the menu named @p name, whether or not a unit owns it yet.
 *
 * The shell's root records the extension and applies it once every unit has
 * registered, so a hand-written unit does not need to know whether the
 * generated unit for its entity registers before it or after it. A root that
 * is not the shell's has no generated unit to extend and no other unit to
 * collide with, so the verbs get a menu of their own.
 */
ORES_SHELL_EXPORT void extend_menu(cli::Menu& root, const std::string& name,
                                   std::function<void(cli::Menu&)> extend);

}

#endif
