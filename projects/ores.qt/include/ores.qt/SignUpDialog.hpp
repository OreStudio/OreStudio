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
#ifndef ORES_QT_SIGNUP_DIALOG_HPP
#define ORES_QT_SIGNUP_DIALOG_HPP

#include <QWidget>
#include <QKeyEvent>
#include <QLineEdit>
#include <QPushButton>
#include <QLabel>
#include <QCheckBox>
#include <QSpinBox>
#include "ores.qt/ClientManager.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::qt {

/**
 * @brief Sign up dialog with dark theme.
 *
 * Provides a clean registration form matching the LoginDialog style.
 * Includes username, email, password with confirmation, and server fields.
 */
class SignUpDialog : public QWidget {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.signup_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit SignUpDialog(QWidget* parent = nullptr);
    ~SignUpDialog() override;

    QSize sizeHint() const override;

    /**
     * @brief Set the server/host field value.
     */
    void setServer(const QString& server);

    /**
     * @brief Set the port field value.
     */
    void setPort(int port);

    /**
     * @brief Get the current server/host field value.
     */
    QString getServer() const;

    /**
     * @brief Get the current port field value.
     */
    int getPort() const;

    /**
     * @brief Set the client manager for performing signup.
     */
    void setClientManager(ClientManager* clientManager);

    /**
     * @brief Get the username that was registered.
     */
    QString getRegisteredUsername() const;

protected:
    void keyPressEvent(QKeyEvent* event) override;

signals:
    /**
     * @brief Emitted when signup and auto-login succeed.
     */
    void loginSucceeded(const QString& username);

    /**
     * @brief Emitted when signup succeeds (before auto-login).
     */
    void signupSucceeded(const QString& username);

    /**
     * @brief Emitted when signup fails.
     */
    void signupFailed(const QString& errorMessage);

    /**
     * @brief Emitted when user wants to go back to login.
     */
    void loginRequested();

    /**
     * @brief Emitted when the widget should be closed.
     */
    void closeRequested();

private slots:
    void onSignUpClicked();
    void onLoginClicked();
    void onShowPasswordToggled(bool checked);
    void onSignUpResult(const SignupResult& result);
    void onLoginResult(const LoginResult& result);

private:
    void setupUI();
    void setupPanel(QWidget* parent);
    void enableForm(bool enabled);
    bool validateInput();

    // UI elements
    QLabel* titleLabel_;
    QLineEdit* usernameEdit_;
    QLineEdit* emailEdit_;
    QLineEdit* passwordEdit_;
    QLineEdit* confirmPasswordEdit_;
    QCheckBox* showPasswordCheck_;
    QPushButton* signUpButton_;
    QPushButton* loginButton_;
    QLabel* loginLabel_;
    QLabel* statusLabel_;

    // Server fields
    QLineEdit* hostEdit_;
    QSpinBox* portSpinBox_;

    // Dependencies
    ClientManager* clientManager_{nullptr};

    // Result tracking
    QString registeredUsername_;
};

}

#endif
