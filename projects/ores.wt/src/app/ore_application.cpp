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
#include "ores.wt/app/ore_application.hpp"
#include <Wt/WBootstrap5Theme.h>
#include <Wt/WNavigationBar.h>
#include <Wt/WMenu.h>
#include <Wt/WStackedWidget.h>
#include <Wt/WText.h>

namespace ores::wt::app {

ore_application::ore_application(const Wt::WEnvironment& env)
    : Wt::WApplication(env) {
    setTitle("ORE Studio");
    setup_theme();
    show_login();
}

ore_application* ore_application::instance() {
    return dynamic_cast<ore_application*>(Wt::WApplication::instance());
}

void ore_application::setup_theme() {
    auto bootstrap_theme = std::make_shared<Wt::WBootstrap5Theme>();
    setTheme(bootstrap_theme);
    useStyleSheet("https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css");
}

void ore_application::show_login() {
    root()->clear();

    auto container = root()->addWidget(
        std::make_unique<Wt::WContainerWidget>());
    container->setStyleClass("container mt-5");

    auto row = container->addWidget(std::make_unique<Wt::WContainerWidget>());
    row->setStyleClass("row justify-content-center");

    auto col = row->addWidget(std::make_unique<Wt::WContainerWidget>());
    col->setStyleClass("col-md-4");

    auto card = col->addWidget(std::make_unique<Wt::WContainerWidget>());
    card->setStyleClass("card");

    auto card_body = card->addWidget(std::make_unique<Wt::WContainerWidget>());
    card_body->setStyleClass("card-body");

    login_widget_ = card_body->addWidget(std::make_unique<login_widget>());
    login_widget_->login_succeeded().connect(this,
        &ore_application::on_login_success);
}

void ore_application::show_main_view() {
    root()->clear();

    auto navbar = root()->addWidget(std::make_unique<Wt::WNavigationBar>());
    navbar->setTitle("ORE Studio", "/");
    navbar->setResponsive(true);
    navbar->addStyleClass("navbar-dark bg-dark");

    auto contents_stack = root()->addWidget(
        std::make_unique<Wt::WStackedWidget>());
    contents_stack->setStyleClass("container mt-3");

    auto left_menu = std::make_unique<Wt::WMenu>(contents_stack);
    auto* menu = left_menu.get();

    auto home_text = std::make_unique<Wt::WText>(
        "<h3>Welcome, " + session_.username + "!</h3>"
        "<p>You are now logged in to ORE Studio.</p>");
    menu->addItem("Home", std::move(home_text));

    auto currencies_text = std::make_unique<Wt::WText>(
        "<p>Currency management coming soon...</p>");
    menu->addItem("Currencies", std::move(currencies_text));

    auto accounts_text = std::make_unique<Wt::WText>(
        "<p>Account management coming soon...</p>");
    menu->addItem("Accounts", std::move(accounts_text));

    navbar->addMenu(std::move(left_menu));

    auto right_menu = std::make_unique<Wt::WMenu>();
    auto logout_item = right_menu->addItem("Logout");
    logout_item->triggered().connect(this, &ore_application::on_logout);
    navbar->addMenu(std::move(right_menu), Wt::AlignmentFlag::Right);
}

void ore_application::on_login_success(const login_result& result) {
    session_.account_id = result.account_id;
    session_.username = result.username;
    session_.email = result.email;
    session_.is_logged_in = true;

    show_main_view();
}

void ore_application::on_logout() {
    session_ = session_info{};
    show_login();
}

}
