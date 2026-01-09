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
#include "ores.wt/app/country_list_widget.hpp"
#include "ores.wt/app/country_dialog.hpp"
#include "ores.wt/app/account_list_widget.hpp"
#include "ores.wt/app/account_dialog.hpp"
#include "ores.wt/service/application_context.hpp"
#include "ores.iam/domain/login_info.hpp"
#include "ores.risk/domain/currency.hpp"
#include "ores.risk/domain/country.hpp"
#include "ores.logging/make_logger.hpp"
#include <Wt/WBootstrap5Theme.h>
#include <Wt/WNavigationBar.h>
#include <Wt/WMenu.h>
#include <Wt/WStackedWidget.h>
#include <Wt/WText.h>
#include <Wt/WMessageBox.h>
#include <Wt/WLabel.h>
#include <Wt/WLineEdit.h>
#include <Wt/WPushButton.h>
#include <Wt/WBreak.h>
#include <unordered_map>
#include <boost/uuid/uuid_io.hpp>
#include <boost/container_hash/hash.hpp>

namespace ores::wt::app {

namespace {

const std::string logger_name = "ores.wt.app.ore_application";
constexpr std::uint32_t max_currencies_to_load = 1000;
constexpr std::uint32_t max_countries_to_load = 1000;

auto& lg() {
    using namespace ores::logging;
    static auto instance = make_logger(logger_name);
    return instance;
}

currency_row to_row(const risk::domain::currency& c) {
    return {c.iso_code, c.name, c.symbol, c.numeric_code, c.currency_type,
            c.version};
}

risk::domain::currency to_domain(const currency_data& d,
                                  const std::string& username) {
    risk::domain::currency c;
    c.version = d.version;
    c.iso_code = d.iso_code;
    c.name = d.name;
    c.numeric_code = d.numeric_code;
    c.symbol = d.symbol;
    c.fraction_symbol = d.fraction_symbol;
    c.fractions_per_unit = d.fractions_per_unit;
    c.rounding_type = d.rounding_type;
    c.rounding_precision = d.rounding_precision;
    c.format = d.format;
    c.currency_type = d.currency_type;
    c.recorded_by = username;
    return c;
}

currency_data to_data(const risk::domain::currency& c) {
    currency_data d;
    d.iso_code = c.iso_code;
    d.name = c.name;
    d.numeric_code = c.numeric_code;
    d.symbol = c.symbol;
    d.fraction_symbol = c.fraction_symbol;
    d.fractions_per_unit = c.fractions_per_unit;
    d.rounding_type = c.rounding_type;
    d.rounding_precision = c.rounding_precision;
    d.format = c.format;
    d.currency_type = c.currency_type;
    d.version = c.version;
    return d;
}

country_row to_country_row(const risk::domain::country& c) {
    return {c.alpha2_code, c.alpha3_code, c.name, c.numeric_code, c.version};
}

risk::domain::country to_country_domain(const country_data& d,
                                         const std::string& username) {
    risk::domain::country c;
    c.version = d.version;
    c.alpha2_code = d.alpha2_code;
    c.alpha3_code = d.alpha3_code;
    c.numeric_code = d.numeric_code;
    c.name = d.name;
    c.official_name = d.official_name;
    c.recorded_by = username;
    return c;
}

country_data to_country_data(const risk::domain::country& c) {
    country_data d;
    d.alpha2_code = c.alpha2_code;
    d.alpha3_code = c.alpha3_code;
    d.numeric_code = c.numeric_code;
    d.name = c.name;
    d.official_name = c.official_name;
    d.version = c.version;
    return d;
}

}

ore_application::ore_application(const Wt::WEnvironment& env)
    : Wt::WApplication(env) {
    setTitle("ORE Studio");
    setup_theme();

    auto& ctx = service::application_context::instance();
    if (ctx.is_bootstrap_mode()) {
        show_bootstrap();
    } else {
        show_login();
    }
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
    login_widget_->login_attempted().connect(this,
        &ore_application::on_login_attempt);
}

void ore_application::show_bootstrap() {
    root()->clear();

    auto container = root()->addWidget(
        std::make_unique<Wt::WContainerWidget>());
    container->setStyleClass("container mt-5");

    auto row = container->addWidget(std::make_unique<Wt::WContainerWidget>());
    row->setStyleClass("row justify-content-center");

    auto col = row->addWidget(std::make_unique<Wt::WContainerWidget>());
    col->setStyleClass("col-md-6");

    auto card = col->addWidget(std::make_unique<Wt::WContainerWidget>());
    card->setStyleClass("card");

    auto card_body = card->addWidget(std::make_unique<Wt::WContainerWidget>());
    card_body->setStyleClass("card-body");

    card_body->addWidget(std::make_unique<Wt::WText>(
        "<h2>ORE Studio Setup</h2>"
        "<p class=\"text-muted\">Create your first administrator account to get started.</p>"));

    auto username_label = card_body->addWidget(
        std::make_unique<Wt::WLabel>("Username:"));
    bootstrap_username_ = card_body->addWidget(std::make_unique<Wt::WLineEdit>());
    bootstrap_username_->setPlaceholderText("Enter admin username");
    username_label->setBuddy(bootstrap_username_);

    card_body->addWidget(std::make_unique<Wt::WBreak>());

    auto email_label = card_body->addWidget(
        std::make_unique<Wt::WLabel>("Email:"));
    bootstrap_email_ = card_body->addWidget(std::make_unique<Wt::WLineEdit>());
    bootstrap_email_->setPlaceholderText("Enter email address");
    email_label->setBuddy(bootstrap_email_);

    card_body->addWidget(std::make_unique<Wt::WBreak>());

    auto password_label = card_body->addWidget(
        std::make_unique<Wt::WLabel>("Password:"));
    bootstrap_password_ = card_body->addWidget(std::make_unique<Wt::WPasswordEdit>());
    bootstrap_password_->setPlaceholderText("Enter password");
    password_label->setBuddy(bootstrap_password_);

    card_body->addWidget(std::make_unique<Wt::WBreak>());

    auto confirm_label = card_body->addWidget(
        std::make_unique<Wt::WLabel>("Confirm Password:"));
    bootstrap_confirm_ = card_body->addWidget(std::make_unique<Wt::WPasswordEdit>());
    bootstrap_confirm_->setPlaceholderText("Confirm password");
    confirm_label->setBuddy(bootstrap_confirm_);

    card_body->addWidget(std::make_unique<Wt::WBreak>());
    card_body->addWidget(std::make_unique<Wt::WBreak>());

    bootstrap_button_ = card_body->addWidget(
        std::make_unique<Wt::WPushButton>("Create Administrator"));
    bootstrap_button_->setStyleClass("btn btn-primary");
    bootstrap_button_->clicked().connect([this] {
        const auto username = bootstrap_username_->text().toUTF8();
        const auto email = bootstrap_email_->text().toUTF8();
        const auto password = bootstrap_password_->text().toUTF8();
        const auto confirm = bootstrap_confirm_->text().toUTF8();

        if (username.empty() || email.empty() || password.empty()) {
            bootstrap_status_->setText("Please fill in all fields.");
            bootstrap_status_->setStyleClass("text-danger mt-2");
            return;
        }

        if (password != confirm) {
            bootstrap_status_->setText("Passwords do not match.");
            bootstrap_status_->setStyleClass("text-danger mt-2");
            return;
        }

        on_bootstrap_create(username, email, password);
    });

    card_body->addWidget(std::make_unique<Wt::WBreak>());

    bootstrap_status_ = card_body->addWidget(std::make_unique<Wt::WText>());
    bootstrap_status_->setStyleClass("mt-2");
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

    std::string username = "User";
    if (session_manager_.session()) {
        username = session_manager_.session()->username;
    }

    auto home_text = std::make_unique<Wt::WText>(
        "<h3>Welcome, " + username + "!</h3>"
        "<p>You are now logged in to ORE Studio.</p>");
    menu->addItem("Home", std::move(home_text));

    auto currency_widget = std::make_unique<currency_list_widget>();
    currency_list_widget_ = currency_widget.get();
    setup_currency_handlers();
    load_currencies();
    menu->addItem("Currencies", std::move(currency_widget));

    auto country_widget = std::make_unique<country_list_widget>();
    country_list_widget_ = country_widget.get();
    setup_country_handlers();
    load_countries();
    menu->addItem("Countries", std::move(country_widget));

    auto account_widget = std::make_unique<account_list_widget>();
    account_list_widget_ = account_widget.get();
    setup_account_handlers();
    load_accounts();
    menu->addItem("Accounts", std::move(account_widget));

    navbar->addMenu(std::move(left_menu));

    auto right_menu = std::make_unique<Wt::WMenu>();
    auto logout_item = right_menu->addItem("Logout");
    logout_item->triggered().connect(this, &ore_application::on_logout);
    navbar->addMenu(std::move(right_menu), Wt::AlignmentFlag::Right);
}

std::string ore_application::get_current_username() const {
    return session_manager_.session()
        ? session_manager_.session()->username
        : "system";
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

void ore_application::load_currencies() {
    auto& ctx = service::application_context::instance();
    if (!ctx.is_initialized()) {
        return;
    }

    using namespace ores::logging;
    try {
        auto currencies = ctx.currency_service().list_currencies(0, max_currencies_to_load);
        std::vector<currency_row> rows;
        rows.reserve(currencies.size());
        for (const auto& c : currencies) {
            rows.push_back(to_row(c));
        }
        currency_list_widget_->set_currencies(rows);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to load currencies: " << e.what();
    }
}

void ore_application::show_add_currency_dialog() {
    using namespace ores::logging;
    auto dialog = addChild(
        std::make_unique<currency_dialog>(currency_dialog::mode::add));

    dialog->saved().connect([this, dialog](const currency_data& data) {
        try {
            auto& ctx = service::application_context::instance();
            auto currency = to_domain(data, get_current_username());
            ctx.currency_service().save_currency(currency);
            load_currencies();
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Failed to add currency: " << e.what();
            auto err_box = addChild(std::make_unique<Wt::WMessageBox>(
                "Error", "Failed to add currency: " + std::string(e.what()),
                Wt::Icon::Critical, Wt::StandardButton::Ok));
            err_box->buttonClicked().connect([this, err_box](Wt::StandardButton) {
                removeChild(err_box);
            });
            err_box->show();
        }
        removeChild(dialog);
    });

    dialog->finished().connect([this, dialog](Wt::DialogCode) {
        removeChild(dialog);
    });

    dialog->show();
}

void ore_application::show_edit_currency_dialog(const std::string& iso_code) {
    using namespace ores::logging;
    auto& ctx = service::application_context::instance();
    auto currency_opt = ctx.currency_service().get_currency(iso_code);
    if (!currency_opt) {
        BOOST_LOG_SEV(lg(), warn) << "Currency not found: " << iso_code;
        auto err_box = addChild(std::make_unique<Wt::WMessageBox>(
            "Not Found", "Currency " + iso_code + " was not found. "
            "It may have been deleted by another user.",
            Wt::Icon::Warning, Wt::StandardButton::Ok));
        err_box->buttonClicked().connect([this, err_box](Wt::StandardButton) {
            removeChild(err_box);
        });
        err_box->show();
        load_currencies();
        return;
    }

    auto dialog = addChild(
        std::make_unique<currency_dialog>(currency_dialog::mode::edit));
    dialog->set_currency(to_data(*currency_opt));

    dialog->saved().connect([this, dialog](const currency_data& data) {
        try {
            auto& ctx = service::application_context::instance();
            auto currency = to_domain(data, get_current_username());
            ctx.currency_service().save_currency(currency);
            load_currencies();
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Failed to update currency: " << e.what();
            auto err_box = addChild(std::make_unique<Wt::WMessageBox>(
                "Error", "Failed to update currency: " + std::string(e.what()),
                Wt::Icon::Critical, Wt::StandardButton::Ok));
            err_box->buttonClicked().connect([this, err_box](Wt::StandardButton) {
                removeChild(err_box);
            });
            err_box->show();
        }
        removeChild(dialog);
    });

    dialog->finished().connect([this, dialog](Wt::DialogCode) {
        removeChild(dialog);
    });

    dialog->show();
}

void ore_application::confirm_delete_currency(const std::string& iso_code) {
    using namespace ores::logging;
    auto msg_box = addChild(std::make_unique<Wt::WMessageBox>(
        "Confirm Delete",
        "Are you sure you want to delete currency " + iso_code + "?",
        Wt::Icon::Warning,
        Wt::StandardButton::Yes | Wt::StandardButton::No));

    msg_box->buttonClicked().connect([this, msg_box, iso_code](Wt::StandardButton btn) {
        if (btn == Wt::StandardButton::Yes) {
            try {
                auto& ctx = service::application_context::instance();
                ctx.currency_service().delete_currency(iso_code);
                load_currencies();
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(lg(), error) << "Failed to delete currency: " << e.what();
                auto err_box = addChild(std::make_unique<Wt::WMessageBox>(
                    "Error", "Failed to delete currency: " + std::string(e.what()),
                    Wt::Icon::Critical, Wt::StandardButton::Ok));
                err_box->buttonClicked().connect([this, err_box](Wt::StandardButton) {
                    removeChild(err_box);
                });
                err_box->show();
            }
        }
        removeChild(msg_box);
    });

    msg_box->show();
}

void ore_application::setup_country_handlers() {
    country_list_widget_->add_requested().connect([this] {
        show_add_country_dialog();
    });

    country_list_widget_->edit_requested().connect([this](const std::string& code) {
        show_edit_country_dialog(code);
    });

    country_list_widget_->delete_requested().connect([this](const std::string& code) {
        confirm_delete_country(code);
    });
}

void ore_application::load_countries() {
    auto& ctx = service::application_context::instance();
    if (!ctx.is_initialized()) {
        return;
    }

    using namespace ores::logging;
    try {
        auto countries = ctx.country_service().list_countries(0, max_countries_to_load);
        std::vector<country_row> rows;
        rows.reserve(countries.size());
        for (const auto& c : countries) {
            rows.push_back(to_country_row(c));
        }
        country_list_widget_->set_countries(rows);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to load countries: " << e.what();
    }
}

void ore_application::show_add_country_dialog() {
    using namespace ores::logging;
    auto dialog = addChild(
        std::make_unique<country_dialog>(country_dialog::mode::add));

    dialog->saved().connect([this, dialog](const country_data& data) {
        try {
            auto& ctx = service::application_context::instance();
            auto country = to_country_domain(data, get_current_username());
            ctx.country_service().save_country(country);
            load_countries();
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Failed to add country: " << e.what();
            auto err_box = addChild(std::make_unique<Wt::WMessageBox>(
                "Error", "Failed to add country: " + std::string(e.what()),
                Wt::Icon::Critical, Wt::StandardButton::Ok));
            err_box->buttonClicked().connect([this, err_box](Wt::StandardButton) {
                removeChild(err_box);
            });
            err_box->show();
        }
        removeChild(dialog);
    });

    dialog->finished().connect([this, dialog](Wt::DialogCode) {
        removeChild(dialog);
    });

    dialog->show();
}

void ore_application::show_edit_country_dialog(const std::string& alpha2_code) {
    using namespace ores::logging;
    auto& ctx = service::application_context::instance();
    auto country_opt = ctx.country_service().get_country(alpha2_code);
    if (!country_opt) {
        BOOST_LOG_SEV(lg(), warn) << "Country not found: " << alpha2_code;
        auto err_box = addChild(std::make_unique<Wt::WMessageBox>(
            "Not Found", "Country " + alpha2_code + " was not found. "
            "It may have been deleted by another user.",
            Wt::Icon::Warning, Wt::StandardButton::Ok));
        err_box->buttonClicked().connect([this, err_box](Wt::StandardButton) {
            removeChild(err_box);
        });
        err_box->show();
        load_countries();
        return;
    }

    auto dialog = addChild(
        std::make_unique<country_dialog>(country_dialog::mode::edit));
    dialog->set_country(to_country_data(*country_opt));

    dialog->saved().connect([this, dialog](const country_data& data) {
        try {
            auto& ctx = service::application_context::instance();
            auto country = to_country_domain(data, get_current_username());
            ctx.country_service().save_country(country);
            load_countries();
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Failed to update country: " << e.what();
            auto err_box = addChild(std::make_unique<Wt::WMessageBox>(
                "Error", "Failed to update country: " + std::string(e.what()),
                Wt::Icon::Critical, Wt::StandardButton::Ok));
            err_box->buttonClicked().connect([this, err_box](Wt::StandardButton) {
                removeChild(err_box);
            });
            err_box->show();
        }
        removeChild(dialog);
    });

    dialog->finished().connect([this, dialog](Wt::DialogCode) {
        removeChild(dialog);
    });

    dialog->show();
}

void ore_application::confirm_delete_country(const std::string& alpha2_code) {
    using namespace ores::logging;
    auto msg_box = addChild(std::make_unique<Wt::WMessageBox>(
        "Confirm Delete",
        "Are you sure you want to delete country " + alpha2_code + "?",
        Wt::Icon::Warning,
        Wt::StandardButton::Yes | Wt::StandardButton::No));

    msg_box->buttonClicked().connect([this, msg_box, alpha2_code](Wt::StandardButton btn) {
        if (btn == Wt::StandardButton::Yes) {
            try {
                auto& ctx = service::application_context::instance();
                ctx.country_service().delete_country(alpha2_code);
                load_countries();
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(lg(), error) << "Failed to delete country: " << e.what();
                auto err_box = addChild(std::make_unique<Wt::WMessageBox>(
                    "Error", "Failed to delete country: " + std::string(e.what()),
                    Wt::Icon::Critical, Wt::StandardButton::Ok));
                err_box->buttonClicked().connect([this, err_box](Wt::StandardButton) {
                    removeChild(err_box);
                });
                err_box->show();
            }
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

void ore_application::load_accounts() {
    using namespace ores::logging;
    auto& ctx = service::application_context::instance();
    if (!ctx.is_initialized()) {
        return;
    }

    try {
        auto accounts = ctx.account_service().list_accounts();
        auto login_infos = ctx.account_service().list_login_info();

        std::unordered_map<boost::uuids::uuid, iam::domain::login_info,
                          boost::hash<boost::uuids::uuid>> login_map;
        for (const auto& li : login_infos) {
            login_map[li.account_id] = li;
        }

        std::vector<account_row> rows;
        rows.reserve(accounts.size());

        for (const auto& acc : accounts) {
            account_row row;
            row.id = acc.id;
            row.username = acc.username;
            row.email = acc.email;
            row.version = acc.version;

            auto it = login_map.find(acc.id);
            if (it != login_map.end()) {
                row.locked = it->second.locked;
                row.online = it->second.online;
                row.failed_logins = it->second.failed_logins;
            }

            rows.push_back(std::move(row));
        }

        account_list_widget_->set_accounts(rows);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to load accounts: " << e.what();
    }
}

void ore_application::show_add_account_dialog() {
    auto dialog = addChild(
        std::make_unique<account_dialog>(account_dialog::mode::add));

    dialog->saved().connect([this, dialog](const account_data&) {
        load_accounts();
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
        load_accounts();
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
            load_accounts();
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
            load_accounts();
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
            load_accounts();
        }
        removeChild(msg_box);
    });

    msg_box->show();
}

void ore_application::on_login_attempt(const std::string& username,
                                       const std::string& password) {
    std::string client_ip = environment().clientAddress();
    if (client_ip.empty()) {
        client_ip = "127.0.0.1";
    }

    auto result = session_manager_.login(username, password, client_ip);

    if (result.success) {
        if (result.password_reset_required) {
            login_widget_->set_status(
                "Password reset required. Please contact administrator.", true);
            login_widget_->enable_form(true);
        } else {
            show_main_view();
        }
    } else {
        login_widget_->set_status(result.error_message, true);
        login_widget_->enable_form(true);
    }
}

void ore_application::on_bootstrap_create(const std::string& username,
                                          const std::string& email,
                                          const std::string& password) {
    auto result = session_manager_.create_bootstrap_admin(username, email, password);

    if (result.success) {
        show_main_view();
    } else {
        bootstrap_status_->setText(result.error_message);
        bootstrap_status_->setStyleClass("text-danger mt-2");
    }
}

void ore_application::on_logout() {
    session_manager_.logout();
    show_login();
}

}
