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
#include "ores.wt/app/country_dialog.hpp"
#include <Wt/WLabel.h>
#include <Wt/WBreak.h>
#include <Wt/WTemplate.h>

namespace ores::wt::app {

country_dialog::country_dialog(mode m)
    : Wt::WDialog(m == mode::add ? "Add Country" : "Edit Country"),
      mode_(m) {
    setModal(true);
    setResizable(true);
    setClosable(true);
    setWidth(Wt::WLength(500));

    setup_form();
    setup_buttons();
}

void country_dialog::setup_form() {
    auto content = contents();
    content->setStyleClass("p-3");

    auto add_field = [&](const std::string& label, auto widget) {
        auto row = content->addWidget(std::make_unique<Wt::WContainerWidget>());
        row->setStyleClass("mb-3");

        auto lbl = row->addWidget(std::make_unique<Wt::WLabel>(label));
        lbl->setStyleClass("form-label");

        auto* ptr = row->addWidget(std::move(widget));
        ptr->setStyleClass("form-control");
        lbl->setBuddy(ptr);
        return ptr;
    };

    alpha2_code_edit_ = add_field("Alpha-2 Code",
        std::make_unique<Wt::WLineEdit>());
    alpha2_code_edit_->setPlaceholderText("e.g., US");
    alpha2_code_edit_->setMaxLength(2);
    if (mode_ == mode::edit) {
        alpha2_code_edit_->setReadOnly(true);
    }

    alpha3_code_edit_ = add_field("Alpha-3 Code",
        std::make_unique<Wt::WLineEdit>());
    alpha3_code_edit_->setPlaceholderText("e.g., USA");
    alpha3_code_edit_->setMaxLength(3);

    numeric_code_edit_ = add_field("Numeric Code",
        std::make_unique<Wt::WLineEdit>());
    numeric_code_edit_->setPlaceholderText("e.g., 840");
    numeric_code_edit_->setMaxLength(3);

    name_edit_ = add_field("Name",
        std::make_unique<Wt::WLineEdit>());
    name_edit_->setPlaceholderText("e.g., United States");

    official_name_edit_ = add_field("Official Name",
        std::make_unique<Wt::WLineEdit>());
    official_name_edit_->setPlaceholderText("e.g., United States of America");

    status_text_ = content->addWidget(std::make_unique<Wt::WText>());
    status_text_->setStyleClass("text-danger");
}

void country_dialog::setup_buttons() {
    auto save_btn = footer()->addWidget(
        std::make_unique<Wt::WPushButton>("Save"));
    save_btn->setStyleClass("btn btn-primary");
    save_btn->clicked().connect(this, &country_dialog::validate_and_save);

    auto cancel_btn = footer()->addWidget(
        std::make_unique<Wt::WPushButton>("Cancel"));
    cancel_btn->setStyleClass("btn btn-secondary ms-2");
    cancel_btn->clicked().connect([this] { reject(); });
}

void country_dialog::set_country(const country_data& data) {
    alpha2_code_edit_->setText(data.alpha2_code);
    alpha3_code_edit_->setText(data.alpha3_code);
    numeric_code_edit_->setText(data.numeric_code);
    name_edit_->setText(data.name);
    official_name_edit_->setText(data.official_name);
}

country_data country_dialog::get_country() const {
    country_data data;
    data.alpha2_code = alpha2_code_edit_->text().toUTF8();
    data.alpha3_code = alpha3_code_edit_->text().toUTF8();
    data.numeric_code = numeric_code_edit_->text().toUTF8();
    data.name = name_edit_->text().toUTF8();
    data.official_name = official_name_edit_->text().toUTF8();
    return data;
}

void country_dialog::validate_and_save() {
    const auto alpha2 = alpha2_code_edit_->text().toUTF8();
    const auto alpha3 = alpha3_code_edit_->text().toUTF8();
    const auto name = name_edit_->text().toUTF8();

    if (alpha2.empty()) {
        status_text_->setText("Alpha-2 Code is required.");
        alpha2_code_edit_->setFocus();
        return;
    }

    if (alpha2.length() != 2) {
        status_text_->setText("Alpha-2 Code must be exactly 2 characters.");
        alpha2_code_edit_->setFocus();
        return;
    }

    if (alpha3.empty()) {
        status_text_->setText("Alpha-3 Code is required.");
        alpha3_code_edit_->setFocus();
        return;
    }

    if (alpha3.length() != 3) {
        status_text_->setText("Alpha-3 Code must be exactly 3 characters.");
        alpha3_code_edit_->setFocus();
        return;
    }

    if (name.empty()) {
        status_text_->setText("Name is required.");
        name_edit_->setFocus();
        return;
    }

    status_text_->setText("");
    on_save();
}

void country_dialog::on_save() {
    saved_.emit(get_country());
    accept();
}

}
