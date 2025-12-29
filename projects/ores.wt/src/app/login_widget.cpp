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
#include "ores.wt/app/login_widget.hpp"
#include <Wt/WBreak.h>
#include <Wt/WLabel.h>

namespace ores::wt::app {

login_widget::login_widget() {
    setStyleClass("login-widget");

    addWidget(std::make_unique<Wt::WText>("<h2>ORE Studio Login</h2>"));

    auto username_label = addWidget(std::make_unique<Wt::WLabel>("Username:"));
    username_edit_ = addWidget(std::make_unique<Wt::WLineEdit>());
    username_edit_->setPlaceholderText("Enter username");
    username_label->setBuddy(username_edit_);

    addWidget(std::make_unique<Wt::WBreak>());

    auto password_label = addWidget(std::make_unique<Wt::WLabel>("Password:"));
    password_edit_ = addWidget(std::make_unique<Wt::WLineEdit>());
    password_edit_->setEchoMode(Wt::EchoMode::Password);
    password_edit_->setPlaceholderText("Enter password");
    password_label->setBuddy(password_edit_);

    addWidget(std::make_unique<Wt::WBreak>());
    addWidget(std::make_unique<Wt::WBreak>());

    auto button_container = addWidget(std::make_unique<Wt::WContainerWidget>());
    button_container->setStyleClass("login-buttons");

    login_button_ = button_container->addWidget(
        std::make_unique<Wt::WPushButton>("Login"));
    login_button_->setStyleClass("btn btn-primary");
    login_button_->clicked().connect(this, &login_widget::on_login_clicked);

    signup_button_ = button_container->addWidget(
        std::make_unique<Wt::WPushButton>("Sign Up"));
    signup_button_->setStyleClass("btn btn-secondary");
    signup_button_->clicked().connect([this] {
        signup_requested_.emit();
    });

    addWidget(std::make_unique<Wt::WBreak>());

    status_text_ = addWidget(std::make_unique<Wt::WText>());
    status_text_->setStyleClass("login-status");

    password_edit_->enterPressed().connect(this, &login_widget::on_login_clicked);
    username_edit_->enterPressed().connect([this] {
        password_edit_->setFocus();
    });
}

void login_widget::on_login_clicked() {
    const auto username = username_edit_->text().toUTF8();
    const auto password = password_edit_->text().toUTF8();

    if (username.empty()) {
        set_status("Please enter a username.", true);
        username_edit_->setFocus();
        return;
    }

    if (password.empty()) {
        set_status("Please enter a password.", true);
        password_edit_->setFocus();
        return;
    }

    set_status("Authenticating...", false);
    enable_form(false);

    login_result result;
    result.success = true;
    result.username = username;
    login_succeeded_.emit(result);
}

void login_widget::set_status(const std::string& message, bool is_error) {
    status_text_->setText(message);
    if (is_error) {
        status_text_->setStyleClass("login-status text-danger");
    } else {
        status_text_->setStyleClass("login-status text-info");
    }
}

void login_widget::enable_form(bool enabled) {
    username_edit_->setEnabled(enabled);
    password_edit_->setEnabled(enabled);
    login_button_->setEnabled(enabled);
    signup_button_->setEnabled(enabled);
}

}
