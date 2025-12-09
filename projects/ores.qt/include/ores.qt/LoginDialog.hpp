/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2024 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_QT_LOGINDIALOG_HPP
#define ORES_QT_LOGINDIALOG_HPP

#include <QLabel>
#include <QDialog>
#include <QLineEdit>
#include <QSpinBox>
#include <QPushButton>
#include "ores.qt/ClientManager.hpp"

namespace ores::qt {

/**
 * @brief Dialog for user authentication and server connection.
 *
 * Presents a login form with username, password, server host, and port fields.
 * Delegates connection and login logic to the provided ClientManager.
 */
class LoginDialog : public QDialog {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.login_dialog";

    static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Construct LoginDialog.
     * @param clientManager Pointer to the application's client manager.
     * @param parent Parent widget.
     */
    explicit LoginDialog(ClientManager* clientManager, QWidget* parent = nullptr);
    ~LoginDialog() override;

    /**
     * @brief Get the logged-in username.
     * @return The username used for login.
     */
    std::string getUsername() const { return username_edit_->text().toStdString(); }

private slots:
    void onLoginClicked();
    void onLoginResult(bool success, const QString& error_message);

signals:
    void loginCompleted(bool success, const QString& error_message);

private:
    void setupUI();
    void enableForm(bool enabled);

private:
    // UI components
    QLineEdit* username_edit_;
    QLineEdit* password_edit_;
    QLineEdit* host_edit_;
    QSpinBox* port_spinbox_;
    QPushButton* login_button_;
    QPushButton* cancel_button_;
    QLabel* status_label_;

    // Dependencies
    ClientManager* clientManager_;
};

}

#endif
