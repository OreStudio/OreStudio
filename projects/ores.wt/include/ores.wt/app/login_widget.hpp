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
#ifndef ORES_WT_APP_LOGIN_WIDGET_HPP
#define ORES_WT_APP_LOGIN_WIDGET_HPP

#include <Wt/WContainerWidget.h>
#include <Wt/WLineEdit.h>
#include <Wt/WPushButton.h>
#include <Wt/WText.h>
#include <Wt/WSignal.h>
#include <boost/uuid/uuid.hpp>

namespace ores::wt::app {

/**
 * @brief Login result containing authentication outcome.
 */
struct login_result {
    bool success = false;
    std::string error_message;
    boost::uuids::uuid account_id;
    std::string username;
    std::string email;
    bool password_reset_required = false;
};

/**
 * @brief Login form widget for user authentication.
 */
class login_widget : public Wt::WContainerWidget {
public:
    login_widget();

    Wt::Signal<login_result>& login_succeeded() { return login_succeeded_; }
    Wt::Signal<>& signup_requested() { return signup_requested_; }

private:
    void on_login_clicked();
    void set_status(const std::string& message, bool is_error);
    void enable_form(bool enabled);

    Wt::WLineEdit* username_edit_;
    Wt::WLineEdit* password_edit_;
    Wt::WPushButton* login_button_;
    Wt::WPushButton* signup_button_;
    Wt::WText* status_text_;

    Wt::Signal<login_result> login_succeeded_;
    Wt::Signal<> signup_requested_;
};

}

#endif
