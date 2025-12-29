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
#include <Wt/WApplication.h>
#include <Wt/WContainerWidget.h>
#include <Wt/WText.h>
#include <Wt/WServer.h>
#include "ores.wt/domain/stub.hpp"

namespace {

/**
 * @brief Main Wt application class.
 */
class ore_application final : public Wt::WApplication {
public:
    explicit ore_application(const Wt::WEnvironment& env)
        : Wt::WApplication(env) {
        setTitle("ORE Studio");
        root()->addWidget(
            std::make_unique<Wt::WText>("Welcome to ORE Studio Web Interface"));
    }
};

/**
 * @brief Factory function to create application instances.
 */
std::unique_ptr<Wt::WApplication>
create_application(const Wt::WEnvironment& env) {
    return std::make_unique<ore_application>(env);
}

}

int main(int argc, char* argv[]) {
    return Wt::WRun(argc, argv, &create_application);
}
