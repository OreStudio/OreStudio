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
#ifndef ORES_WT_APP_CURRENCY_DIALOG_HPP
#define ORES_WT_APP_CURRENCY_DIALOG_HPP

#include <Wt/WDialog.h>
#include <Wt/WLineEdit.h>
#include <Wt/WSpinBox.h>
#include <Wt/WComboBox.h>
#include <Wt/WPushButton.h>
#include <Wt/WText.h>
#include <Wt/WSignal.h>

namespace ores::wt::app {

/**
 * @brief Currency data for form binding.
 */
struct currency_data {
    std::string iso_code;
    std::string name;
    std::string numeric_code;
    std::string symbol;
    std::string fraction_symbol;
    int fractions_per_unit = 100;
    std::string rounding_type = "Closest";
    int rounding_precision = 2;
    std::string format;
    std::string monetary_nature = "Fiat";
    std::string market_tier = "G10";
    int version = 0;
};

/**
 * @brief Dialog for creating/editing currencies.
 */
class currency_dialog : public Wt::WDialog {
public:
    enum class mode { add, edit };

    explicit currency_dialog(mode m);

    void set_currency(const currency_data& data);
    currency_data get_currency() const;

    Wt::Signal<currency_data>& saved() { return saved_; }

private:
    void setup_form();
    void setup_buttons();
    void on_save();
    void validate_and_save();

    mode mode_;
    Wt::WLineEdit* iso_code_edit_;
    Wt::WLineEdit* name_edit_;
    Wt::WLineEdit* numeric_code_edit_;
    Wt::WLineEdit* symbol_edit_;
    Wt::WLineEdit* fraction_symbol_edit_;
    Wt::WSpinBox* fractions_spinbox_;
    Wt::WComboBox* rounding_type_combo_;
    Wt::WSpinBox* precision_spinbox_;
    Wt::WLineEdit* format_edit_;
    Wt::WComboBox* monetary_nature_combo_;
    Wt::WComboBox* market_tier_combo_;
    Wt::WText* status_text_;

    Wt::Signal<currency_data> saved_;
};

}

#endif
