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
#ifndef ORES_WT_APP_COUNTRY_DIALOG_HPP
#define ORES_WT_APP_COUNTRY_DIALOG_HPP

#include <Wt/WDialog.h>
#include <Wt/WLineEdit.h>
#include <Wt/WPushButton.h>
#include <Wt/WText.h>
#include <Wt/WSignal.h>

namespace ores::wt::app {

/**
 * @brief Country data for form binding.
 */
struct country_data {
    std::string alpha2_code;
    std::string alpha3_code;
    std::string numeric_code;
    std::string name;
    std::string official_name;
    int version = 0;
};

/**
 * @brief Dialog for creating/editing countries.
 */
class country_dialog : public Wt::WDialog {
public:
    enum class mode { add, edit };

    explicit country_dialog(mode m);

    void set_country(const country_data& data);
    country_data get_country() const;

    Wt::Signal<country_data>& saved() { return saved_; }

private:
    void setup_form();
    void setup_buttons();
    void on_save();
    void validate_and_save();

    mode mode_;
    Wt::WLineEdit* alpha2_code_edit_;
    Wt::WLineEdit* alpha3_code_edit_;
    Wt::WLineEdit* numeric_code_edit_;
    Wt::WLineEdit* name_edit_;
    Wt::WLineEdit* official_name_edit_;
    Wt::WText* status_text_;

    Wt::Signal<country_data> saved_;
};

}

#endif
