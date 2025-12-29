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

    auto add_field = [&](const std::string& label, auto& widget) {
        auto row = content->addWidget(std::make_unique<Wt::WContainerWidget>());
        row->setStyleClass("mb-3");

        auto lbl = row->addWidget(std::make_unique<Wt::WLabel>(label));
        lbl->setStyleClass("form-label");

        widget = row->addWidget(std::move(widget));
        widget->setStyleClass("form-control");
        lbl->setBuddy(widget);
    };

    auto iso_edit = std::make_unique<Wt::WLineEdit>();
    iso_edit->setPlaceholderText("e.g., USD");
    iso_edit->setMaxLength(3);
    add_field("ISO Code", iso_edit);
    iso_code_edit_ = iso_edit.get();
    if (mode_ == mode::edit) {
        iso_code_edit_->setReadOnly(true);
    }

    auto name_edit = std::make_unique<Wt::WLineEdit>();
    name_edit->setPlaceholderText("e.g., United States Dollar");
    add_field("Name", name_edit);
    name_edit_ = name_edit.get();

    auto numeric_edit = std::make_unique<Wt::WLineEdit>();
    numeric_edit->setPlaceholderText("e.g., 840");
    numeric_edit->setMaxLength(3);
    add_field("Numeric Code", numeric_edit);
    numeric_code_edit_ = numeric_edit.get();

    auto symbol_edit = std::make_unique<Wt::WLineEdit>();
    symbol_edit->setPlaceholderText("e.g., $");
    add_field("Symbol", symbol_edit);
    symbol_edit_ = symbol_edit.get();

    auto fraction_edit = std::make_unique<Wt::WLineEdit>();
    fraction_edit->setPlaceholderText("e.g., ¢");
    add_field("Fraction Symbol", fraction_edit);
    fraction_symbol_edit_ = fraction_edit.get();

    auto fractions_spin = std::make_unique<Wt::WSpinBox>();
    fractions_spin->setRange(1, 10000);
    fractions_spin->setValue(100);
    add_field("Fractions Per Unit", fractions_spin);
    fractions_spinbox_ = fractions_spin.get();

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

    auto precision_spin = std::make_unique<Wt::WSpinBox>();
    precision_spin->setRange(0, 10);
    precision_spin->setValue(2);
    add_field("Rounding Precision", precision_spin);
    precision_spinbox_ = precision_spin.get();

    auto format_edit = std::make_unique<Wt::WLineEdit>();
    format_edit->setPlaceholderText("e.g., #,##0.00");
    add_field("Format", format_edit);
    format_edit_ = format_edit.get();

    auto type_row = content->addWidget(
        std::make_unique<Wt::WContainerWidget>());
    type_row->setStyleClass("mb-3");
    auto type_lbl = type_row->addWidget(
        std::make_unique<Wt::WLabel>("Currency Type"));
    type_lbl->setStyleClass("form-label");
    currency_type_combo_ = type_row->addWidget(
        std::make_unique<Wt::WComboBox>());
    currency_type_combo_->setStyleClass("form-select");
    currency_type_combo_->addItem("Fiat");
    currency_type_combo_->addItem("Crypto");
    currency_type_combo_->addItem("Commodity");
    currency_type_combo_->addItem("Other");
    type_lbl->setBuddy(currency_type_combo_);

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

    int type_idx = currency_type_combo_->findText(data.currency_type);
    if (type_idx >= 0) {
        currency_type_combo_->setCurrentIndex(type_idx);
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
    data.currency_type = currency_type_combo_->currentText().toUTF8();
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
