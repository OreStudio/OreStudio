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
#ifndef ORES_QT_SIGNUPDIALOG_HPP
#define ORES_QT_SIGNUPDIALOG_HPP

#include <QLabel>
#include <QDialog>
#include <QLineEdit>
#include <QSpinBox>
#include <QPushButton>
#include "ores.qt/ClientManager.hpp"

namespace ores::qt {

/**
 * @brief Dialog for user self-registration.
 *
 * Presents a signup form with username, email, password, and server fields.
 * Delegates registration logic to the provided ClientManager.
 */
class SignUpDialog : public QDialog {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.signup_dialog";

    static auto& lg() {
        using namespace ores::telemetry::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Construct SignUpDialog.
     * @param clientManager Pointer to the application's client manager.
     * @param parent Parent widget.
     */
    explicit SignUpDialog(ClientManager* clientManager, QWidget* parent = nullptr);
    ~SignUpDialog() override;

    /**
     * @brief Set the server host and port fields.
     *
     * Useful when opening from LoginDialog to pre-fill server info.
     */
    void setServerInfo(const QString& host, int port);

    /**
     * @brief Get the registered username after successful signup.
     * @return The username that was registered.
     */
    QString getRegisteredUsername() const { return registered_username_; }

private slots:
    void onSignUpClicked();
    void onSignUpResult(const SignupResult& result);
    void updatePasswordMatchIndicator();

signals:
    void signupCompleted(const SignupResult& result);

private:
    void setupUI();
    void enableForm(bool enabled);
    bool validateInput();

private:
    // UI components
    QLineEdit* username_edit_;
    QLineEdit* email_edit_;
    QLineEdit* password_edit_;
    QLineEdit* confirm_password_edit_;
    QLineEdit* host_edit_;
    QSpinBox* port_spinbox_;
    QPushButton* signup_button_;
    QPushButton* cancel_button_;
    QLabel* status_label_;

    // Dependencies
    ClientManager* clientManager_;

    // Result tracking
    QString registered_username_;
};

}

#endif
