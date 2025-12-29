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
#ifndef ORES_WT_APP_ACCOUNT_DIALOG_HPP
#define ORES_WT_APP_ACCOUNT_DIALOG_HPP

#include <Wt/WDialog.h>
#include <Wt/WLineEdit.h>
#include <Wt/WPushButton.h>
#include <Wt/WText.h>
#include <Wt/WSignal.h>
#include <boost/uuid/uuid.hpp>

namespace ores::wt::app {

/**
 * @brief Account data for form binding.
 */
struct account_data {
    boost::uuids::uuid id;
    std::string username;
    std::string email;
    std::string password;
    std::string confirm_password;
    int version = 0;
};

/**
 * @brief Dialog for creating/editing user accounts.
 */
class account_dialog : public Wt::WDialog {
public:
    enum class mode { add, edit };

    explicit account_dialog(mode m);

    void set_account(const account_data& data);
    account_data get_account() const;

    Wt::Signal<account_data>& saved() { return saved_; }

private:
    void setup_form();
    void setup_buttons();
    void validate_and_save();

    mode mode_;
    Wt::WLineEdit* username_edit_;
    Wt::WLineEdit* email_edit_;
    Wt::WLineEdit* password_edit_;
    Wt::WLineEdit* confirm_password_edit_;
    Wt::WText* status_text_;

    boost::uuids::uuid account_id_;
    Wt::Signal<account_data> saved_;
};

}

#endif
