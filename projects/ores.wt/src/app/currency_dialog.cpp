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
#include "ores.wt/app/currency_dialog.hpp"
#include <Wt/WLabel.h>
#include <Wt/WBreak.h>
#include <Wt/WTemplate.h>

namespace ores::wt::app {

currency_dialog::currency_dialog(mode m)
    : Wt::WDialog(m == mode::add ? "Add Currency" : "Edit Currency"),
      mode_(m) {
    setModal(true);
    setResizable(true);
    setClosable(true);
    setWidth(Wt::WLength(500));

    setup_form();
    setup_buttons();
}

void currency_dialog::setup_form() {
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

    iso_code_edit_ = add_field("ISO Code",
        std::make_unique<Wt::WLineEdit>());
    iso_code_edit_->setPlaceholderText("e.g., USD");
    iso_code_edit_->setMaxLength(3);
    if (mode_ == mode::edit) {
        iso_code_edit_->setReadOnly(true);
    }

    name_edit_ = add_field("Name",
        std::make_unique<Wt::WLineEdit>());
    name_edit_->setPlaceholderText("e.g., United States Dollar");

    numeric_code_edit_ = add_field("Numeric Code",
        std::make_unique<Wt::WLineEdit>());
    numeric_code_edit_->setPlaceholderText("e.g., 840");
    numeric_code_edit_->setMaxLength(3);

    symbol_edit_ = add_field("Symbol",
        std::make_unique<Wt::WLineEdit>());
    symbol_edit_->setPlaceholderText("e.g., $");

    fraction_symbol_edit_ = add_field("Fraction Symbol",
        std::make_unique<Wt::WLineEdit>());
    fraction_symbol_edit_->setPlaceholderText("e.g., ¢");

    fractions_spinbox_ = add_field("Fractions Per Unit",
        std::make_unique<Wt::WSpinBox>());
    fractions_spinbox_->setRange(1, 10000);
    fractions_spinbox_->setValue(100);

    auto rounding_row = content->addWidget(
        std::make_unique<Wt::WContainerWidget>());
    rounding_row->setStyleClass("mb-3");
    auto rounding_lbl = rounding_row->addWidget(
        std::make_unique<Wt::WLabel>("Rounding Type"));
    rounding_lbl->setStyleClass("form-label");
    rounding_type_combo_ = rounding_row->addWidget(
        std::make_unique<Wt::WComboBox>());
    rounding_type_combo_->setStyleClass("form-select");
    rounding_type_combo_->addItem("Closest");
    rounding_type_combo_->addItem("Up");
    rounding_type_combo_->addItem("Down");
    rounding_type_combo_->addItem("Floor");
    rounding_type_combo_->addItem("Ceiling");
    rounding_lbl->setBuddy(rounding_type_combo_);

    precision_spinbox_ = add_field("Rounding Precision",
        std::make_unique<Wt::WSpinBox>());
    precision_spinbox_->setRange(0, 10);
    precision_spinbox_->setValue(2);

    format_edit_ = add_field("Format",
        std::make_unique<Wt::WLineEdit>());
    format_edit_->setPlaceholderText("e.g., #,##0.00");

    auto monetary_nature_row = content->addWidget(
        std::make_unique<Wt::WContainerWidget>());
    monetary_nature_row->setStyleClass("mb-3");
    auto monetary_nature_lbl = monetary_nature_row->addWidget(
        std::make_unique<Wt::WLabel>("Asset Class"));
    monetary_nature_lbl->setStyleClass("form-label");
    monetary_nature_combo_ = monetary_nature_row->addWidget(
        std::make_unique<Wt::WComboBox>());
    monetary_nature_combo_->setStyleClass("form-select");
    monetary_nature_combo_->addItem("Fiat");
    monetary_nature_combo_->addItem("Crypto");
    monetary_nature_combo_->addItem("Commodity");
    monetary_nature_combo_->addItem("Other");
    monetary_nature_lbl->setBuddy(monetary_nature_combo_);

    auto market_tier_row = content->addWidget(
        std::make_unique<Wt::WContainerWidget>());
    market_tier_row->setStyleClass("mb-3");
    auto market_tier_lbl = market_tier_row->addWidget(
        std::make_unique<Wt::WLabel>("Market Tier"));
    market_tier_lbl->setStyleClass("form-label");
    market_tier_combo_ = market_tier_row->addWidget(
        std::make_unique<Wt::WComboBox>());
    market_tier_combo_->setStyleClass("form-select");
    market_tier_combo_->addItem("G10");
    market_tier_combo_->addItem("Major");
    market_tier_combo_->addItem("Minor");
    market_tier_combo_->addItem("Exotic");
    market_tier_combo_->addItem("Other");
    market_tier_lbl->setBuddy(market_tier_combo_);

    status_text_ = content->addWidget(std::make_unique<Wt::WText>());
    status_text_->setStyleClass("text-danger");
}

void currency_dialog::setup_buttons() {
    auto save_btn = footer()->addWidget(
        std::make_unique<Wt::WPushButton>("Save"));
    save_btn->setStyleClass("btn btn-primary");
    save_btn->clicked().connect(this, &currency_dialog::validate_and_save);

    auto cancel_btn = footer()->addWidget(
        std::make_unique<Wt::WPushButton>("Cancel"));
    cancel_btn->setStyleClass("btn btn-secondary ms-2");
    cancel_btn->clicked().connect([this] { reject(); });
}

void currency_dialog::set_currency(const currency_data& data) {
    iso_code_edit_->setText(data.iso_code);
    name_edit_->setText(data.name);
    numeric_code_edit_->setText(data.numeric_code);
    symbol_edit_->setText(data.symbol);
    fraction_symbol_edit_->setText(data.fraction_symbol);
    fractions_spinbox_->setValue(data.fractions_per_unit);
    format_edit_->setText(data.format);
    precision_spinbox_->setValue(data.rounding_precision);

    int rounding_idx = rounding_type_combo_->findText(data.rounding_type);
    if (rounding_idx >= 0) {
        rounding_type_combo_->setCurrentIndex(rounding_idx);
    }

    int monetary_nature_idx = monetary_nature_combo_->findText(data.monetary_nature);
    if (monetary_nature_idx >= 0) {
        monetary_nature_combo_->setCurrentIndex(monetary_nature_idx);
    }

    int market_tier_idx = market_tier_combo_->findText(data.market_tier);
    if (market_tier_idx >= 0) {
        market_tier_combo_->setCurrentIndex(market_tier_idx);
    }
}

currency_data currency_dialog::get_currency() const {
    currency_data data;
    data.iso_code = iso_code_edit_->text().toUTF8();
    data.name = name_edit_->text().toUTF8();
    data.numeric_code = numeric_code_edit_->text().toUTF8();
    data.symbol = symbol_edit_->text().toUTF8();
    data.fraction_symbol = fraction_symbol_edit_->text().toUTF8();
    data.fractions_per_unit = fractions_spinbox_->value();
    data.rounding_type = rounding_type_combo_->currentText().toUTF8();
    data.rounding_precision = precision_spinbox_->value();
    data.format = format_edit_->text().toUTF8();
    data.monetary_nature = monetary_nature_combo_->currentText().toUTF8();
    data.market_tier = market_tier_combo_->currentText().toUTF8();
    return data;
}

void currency_dialog::validate_and_save() {
    const auto iso = iso_code_edit_->text().toUTF8();
    const auto name = name_edit_->text().toUTF8();

    if (iso.empty()) {
        status_text_->setText("ISO Code is required.");
        iso_code_edit_->setFocus();
        return;
    }

    if (iso.length() != 3) {
        status_text_->setText("ISO Code must be exactly 3 characters.");
        iso_code_edit_->setFocus();
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

void currency_dialog::on_save() {
    saved_.emit(get_currency());
    accept();
}

}
