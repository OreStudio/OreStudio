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
#include "ores.wt/app/currency_list_widget.hpp"
#include "ores.wt/app/currency_dialog.hpp"
#include "ores.wt/app/account_list_widget.hpp"
#include "ores.wt/app/account_dialog.hpp"
#include <Wt/WBootstrap5Theme.h>
#include <Wt/WNavigationBar.h>
#include <Wt/WMenu.h>
#include <Wt/WStackedWidget.h>
#include <Wt/WText.h>
#include <Wt/WMessageBox.h>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>

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

    auto currency_widget = std::make_unique<currency_list_widget>();
    currency_list_widget_ = currency_widget.get();
    setup_currency_handlers();
    load_sample_currencies();
    menu->addItem("Currencies", std::move(currency_widget));

    auto account_widget = std::make_unique<account_list_widget>();
    account_list_widget_ = account_widget.get();
    setup_account_handlers();
    load_sample_accounts();
    menu->addItem("Accounts", std::move(account_widget));

    navbar->addMenu(std::move(left_menu));

    auto right_menu = std::make_unique<Wt::WMenu>();
    auto logout_item = right_menu->addItem("Logout");
    logout_item->triggered().connect(this, &ore_application::on_logout);
    navbar->addMenu(std::move(right_menu), Wt::AlignmentFlag::Right);
}

void ore_application::setup_currency_handlers() {
    currency_list_widget_->add_requested().connect([this] {
        show_add_currency_dialog();
    });

    currency_list_widget_->edit_requested().connect([this](const std::string& iso) {
        show_edit_currency_dialog(iso);
    });

    currency_list_widget_->delete_requested().connect([this](const std::string& iso) {
        confirm_delete_currency(iso);
    });
}

void ore_application::load_sample_currencies() {
    std::vector<currency_row> currencies = {
        {"USD", "United States Dollar", "$", "840", "Fiat", 1},
        {"EUR", "Euro", "€", "978", "Fiat", 1},
        {"GBP", "British Pound Sterling", "£", "826", "Fiat", 1},
        {"JPY", "Japanese Yen", "¥", "392", "Fiat", 1},
        {"CHF", "Swiss Franc", "CHF", "756", "Fiat", 1},
        {"BTC", "Bitcoin", "₿", "", "Crypto", 1},
    };
    currency_list_widget_->set_currencies(currencies);
}

void ore_application::show_add_currency_dialog() {
    auto dialog = addChild(
        std::make_unique<currency_dialog>(currency_dialog::mode::add));

    dialog->saved().connect([this, dialog](const currency_data&) {
        load_sample_currencies();
        removeChild(dialog);
    });

    dialog->finished().connect([this, dialog](Wt::DialogCode) {
        removeChild(dialog);
    });

    dialog->show();
}

void ore_application::show_edit_currency_dialog(const std::string& iso_code) {
    auto dialog = addChild(
        std::make_unique<currency_dialog>(currency_dialog::mode::edit));

    currency_data data;
    data.iso_code = iso_code;
    data.name = iso_code + " Currency";
    data.symbol = "$";
    data.numeric_code = "000";
    data.currency_type = "Fiat";
    dialog->set_currency(data);

    dialog->saved().connect([this, dialog](const currency_data&) {
        load_sample_currencies();
        removeChild(dialog);
    });

    dialog->finished().connect([this, dialog](Wt::DialogCode) {
        removeChild(dialog);
    });

    dialog->show();
}

void ore_application::confirm_delete_currency(const std::string& iso_code) {
    auto msg_box = addChild(std::make_unique<Wt::WMessageBox>(
        "Confirm Delete",
        "Are you sure you want to delete currency " + iso_code + "?",
        Wt::Icon::Warning,
        Wt::StandardButton::Yes | Wt::StandardButton::No));

    msg_box->buttonClicked().connect([this, msg_box](Wt::StandardButton btn) {
        if (btn == Wt::StandardButton::Yes) {
            load_sample_currencies();
        }
        removeChild(msg_box);
    });

    msg_box->show();
}

void ore_application::setup_account_handlers() {
    account_list_widget_->add_requested().connect([this] {
        show_add_account_dialog();
    });

    account_list_widget_->edit_requested().connect(
        [this](const boost::uuids::uuid& id) {
            show_edit_account_dialog(id);
        });

    account_list_widget_->delete_requested().connect(
        [this](const boost::uuids::uuid& id) {
            confirm_delete_account(id);
        });

    account_list_widget_->lock_requested().connect(
        [this](const boost::uuids::uuid& id) {
            confirm_lock_account(id);
        });

    account_list_widget_->unlock_requested().connect(
        [this](const boost::uuids::uuid& id) {
            confirm_unlock_account(id);
        });
}

void ore_application::load_sample_accounts() {
    boost::uuids::random_generator gen;
    std::vector<account_row> accounts = {
        {gen(), "admin", "admin@example.com", false, true, 0, 1},
        {gen(), "john.doe", "john.doe@example.com", false, false, 0, 1},
        {gen(), "jane.smith", "jane.smith@example.com", false, true, 0, 2},
        {gen(), "locked_user", "locked@example.com", true, false, 5, 1},
    };
    account_list_widget_->set_accounts(accounts);
}

void ore_application::show_add_account_dialog() {
    auto dialog = addChild(
        std::make_unique<account_dialog>(account_dialog::mode::add));

    dialog->saved().connect([this, dialog](const account_data&) {
        load_sample_accounts();
        removeChild(dialog);
    });

    dialog->finished().connect([this, dialog](Wt::DialogCode) {
        removeChild(dialog);
    });

    dialog->show();
}

void ore_application::show_edit_account_dialog(const boost::uuids::uuid& id) {
    auto dialog = addChild(
        std::make_unique<account_dialog>(account_dialog::mode::edit));

    account_data data;
    data.id = id;
    data.username = "sample_user";
    data.email = "sample@example.com";
    dialog->set_account(data);

    dialog->saved().connect([this, dialog](const account_data&) {
        load_sample_accounts();
        removeChild(dialog);
    });

    dialog->finished().connect([this, dialog](Wt::DialogCode) {
        removeChild(dialog);
    });

    dialog->show();
}

void ore_application::confirm_delete_account(const boost::uuids::uuid& id) {
    auto msg_box = addChild(std::make_unique<Wt::WMessageBox>(
        "Confirm Delete",
        "Are you sure you want to delete this account?",
        Wt::Icon::Warning,
        Wt::StandardButton::Yes | Wt::StandardButton::No));

    msg_box->buttonClicked().connect([this, msg_box, id](Wt::StandardButton btn) {
        if (btn == Wt::StandardButton::Yes) {
            load_sample_accounts();
        }
        removeChild(msg_box);
    });

    msg_box->show();
}

void ore_application::confirm_lock_account(const boost::uuids::uuid& id) {
    auto msg_box = addChild(std::make_unique<Wt::WMessageBox>(
        "Confirm Lock",
        "Are you sure you want to lock this account?",
        Wt::Icon::Warning,
        Wt::StandardButton::Yes | Wt::StandardButton::No));

    msg_box->buttonClicked().connect([this, msg_box, id](Wt::StandardButton btn) {
        if (btn == Wt::StandardButton::Yes) {
            load_sample_accounts();
        }
        removeChild(msg_box);
    });

    msg_box->show();
}

void ore_application::confirm_unlock_account(const boost::uuids::uuid& id) {
    auto msg_box = addChild(std::make_unique<Wt::WMessageBox>(
        "Confirm Unlock",
        "Are you sure you want to unlock this account?",
        Wt::Icon::Information,
        Wt::StandardButton::Yes | Wt::StandardButton::No));

    msg_box->buttonClicked().connect([this, msg_box, id](Wt::StandardButton btn) {
        if (btn == Wt::StandardButton::Yes) {
            load_sample_accounts();
        }
        removeChild(msg_box);
    });

    msg_box->show();
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
