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
#include "ores.wt/app/account_dialog.hpp"
#include <Wt/WLabel.h>
#include <Wt/WBreak.h>

namespace ores::wt::app {

account_dialog::account_dialog(mode m)
    : Wt::WDialog(m == mode::add ? "Add Account" : "Edit Account"),
      mode_(m) {
    setModal(true);
    setResizable(true);
    setClosable(true);
    setWidth(Wt::WLength(450));

    setup_form();
    setup_buttons();
}

void account_dialog::setup_form() {
    auto content = contents();
    content->setStyleClass("p-3");

    auto add_field = [&](const std::string& label, auto& widget,
                         bool is_password = false) {
        auto row = content->addWidget(std::make_unique<Wt::WContainerWidget>());
        row->setStyleClass("mb-3");

        auto lbl = row->addWidget(std::make_unique<Wt::WLabel>(label));
        lbl->setStyleClass("form-label");

        widget = row->addWidget(std::move(widget));
        widget->setStyleClass("form-control");
        if (is_password) {
            widget->setEchoMode(Wt::EchoMode::Password);
        }
        lbl->setBuddy(widget);
    };

    auto username_edit = std::make_unique<Wt::WLineEdit>();
    username_edit->setPlaceholderText("Enter username");
    add_field("Username", username_edit);
    username_edit_ = username_edit.get();
    if (mode_ == mode::edit) {
        username_edit_->setReadOnly(true);
    }

    auto email_edit = std::make_unique<Wt::WLineEdit>();
    email_edit->setPlaceholderText("user@example.com");
    add_field("Email", email_edit);
    email_edit_ = email_edit.get();

    auto password_edit = std::make_unique<Wt::WLineEdit>();
    password_edit->setPlaceholderText(
        mode_ == mode::add ? "Enter password" : "Leave blank to keep current");
    add_field("Password", password_edit, true);
    password_edit_ = password_edit.get();

    auto confirm_edit = std::make_unique<Wt::WLineEdit>();
    confirm_edit->setPlaceholderText("Confirm password");
    add_field("Confirm Password", confirm_edit, true);
    confirm_password_edit_ = confirm_edit.get();

    status_text_ = content->addWidget(std::make_unique<Wt::WText>());
    status_text_->setStyleClass("text-danger");
}

void account_dialog::setup_buttons() {
    auto save_btn = footer()->addWidget(
        std::make_unique<Wt::WPushButton>("Save"));
    save_btn->setStyleClass("btn btn-primary");
    save_btn->clicked().connect(this, &account_dialog::validate_and_save);

    auto cancel_btn = footer()->addWidget(
        std::make_unique<Wt::WPushButton>("Cancel"));
    cancel_btn->setStyleClass("btn btn-secondary ms-2");
    cancel_btn->clicked().connect([this] { reject(); });
}

void account_dialog::set_account(const account_data& data) {
    account_id_ = data.id;
    username_edit_->setText(data.username);
    email_edit_->setText(data.email);
}

account_data account_dialog::get_account() const {
    account_data data;
    data.id = account_id_;
    data.username = username_edit_->text().toUTF8();
    data.email = email_edit_->text().toUTF8();
    data.password = password_edit_->text().toUTF8();
    data.confirm_password = confirm_password_edit_->text().toUTF8();
    return data;
}

void account_dialog::validate_and_save() {
    const auto username = username_edit_->text().toUTF8();
    const auto email = email_edit_->text().toUTF8();
    const auto password = password_edit_->text().toUTF8();
    const auto confirm = confirm_password_edit_->text().toUTF8();

    if (username.empty()) {
        status_text_->setText("Username is required.");
        username_edit_->setFocus();
        return;
    }

    if (email.empty()) {
        status_text_->setText("Email is required.");
        email_edit_->setFocus();
        return;
    }

    if (email.find('@') == std::string::npos) {
        status_text_->setText("Please enter a valid email address.");
        email_edit_->setFocus();
        return;
    }

    if (mode_ == mode::add && password.empty()) {
        status_text_->setText("Password is required for new accounts.");
        password_edit_->setFocus();
        return;
    }

    if (!password.empty()) {
        if (password.length() < 8) {
            status_text_->setText("Password must be at least 8 characters.");
            password_edit_->setFocus();
            return;
        }

        if (password != confirm) {
            status_text_->setText("Passwords do not match.");
            confirm_password_edit_->setFocus();
            return;
        }
    }

    status_text_->setText("");
    saved_.emit(get_account());
    accept();
}

}
