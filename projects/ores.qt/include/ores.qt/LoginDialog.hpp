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
#ifndef ORES_QT_LOGIN_DIALOG_HPP
#define ORES_QT_LOGIN_DIALOG_HPP

#include <QWidget>
#include <QKeyEvent>
#include <QLineEdit>
#include <QPushButton>
#include <QToolButton>
#include <QLabel>
#include <QCheckBox>
#include <QComboBox>
#include <QSpinBox>
#include <QMenu>
#include <QVBoxLayout>
#include "ores.qt/ClientManager.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::qt {

/**
 * @brief Login dialog with dark theme.
 *
 * Provides a clean login form with username, password, and server fields.
 * Supports saved connections via a dropdown menu.
 */
class LoginDialog : public QWidget {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.login_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit LoginDialog(QWidget* parent = nullptr);
    ~LoginDialog() override;

    QSize sizeHint() const override;

    /**
     * @brief Set the list of saved connection names for the dropdown.
     */
    void setSavedConnections(const QStringList& connectionNames);

    /**
     * @brief Set the server/host field value.
     */
    void setServer(const QString& server);

    /**
     * @brief Set the port field value.
     */
    void setPort(int port);

    /**
     * @brief Set the username field value.
     */
    void setUsername(const QString& username);

    /**
     * @brief Set the password field value.
     */
    void setPassword(const QString& password);

    /**
     * @brief Set the client manager for performing login.
     */
    void setClientManager(ClientManager* clientManager);

    /**
     * @brief Get the username that was used for login.
     */
    QString getUsername() const;

    /**
     * @brief Get the password that was entered for login.
     */
    QString getPassword() const;

    /**
     * @brief Get the current server/host field value.
     */
    QString getServer() const;

    /**
     * @brief Get the current port field value.
     */
    int getPort() const;

protected:
    void keyPressEvent(QKeyEvent* event) override;

signals:
    /**
     * @brief Emitted when login succeeds.
     */
    void loginSucceeded(const QString& username);

    /**
     * @brief Emitted when login fails.
     */
    void loginFailed(const QString& errorMessage);

    void signUpRequested();
    void closeRequested();

    /**
     * @brief Emitted when user selects a saved connection from the dropdown.
     */
    void savedConnectionSelected(const QString& connectionName);

    /**
     * @brief Emitted when the server is in bootstrap mode.
     *
     * This signal indicates that the system has no administrator account yet
     * and the SystemProvisionerWizard should be shown instead of the login form.
     */
    void bootstrapModeDetected();

    /**
     * @brief Emitted when the tenant is in bootstrap mode.
     *
     * This signal indicates that the authenticated tenant needs initial setup
     * and the TenantProvisioningWizard should be shown after login.
     */
    void tenantBootstrapDetected();

private slots:
    void onLoginClicked();
    void onSignUpClicked();
    void onGetStartedClicked();
    void onShowPasswordToggled(bool checked);
    void onLoginResult(const LoginResult& result);

private:
    void setupUI();
    void setupLeftPanel(QWidget* parent);
    void setupRightPanel(QWidget* parent);
    void setupHeader(QVBoxLayout* layout, QWidget* parent);
    void setupAuthFields(QVBoxLayout* layout, QWidget* parent);
    void setupServerFields(QVBoxLayout* layout, QWidget* parent);
    void setupActions(QVBoxLayout* layout, QWidget* parent);
    void setupFooter(QVBoxLayout* layout, QWidget* parent);
    void enableForm(bool enabled);

    // UI elements
    QLabel* loginTitleLabel_;
    QLineEdit* usernameEdit_;
    QLineEdit* passwordEdit_;
    QCheckBox* showPasswordCheck_;
    QCheckBox* rememberMeCheck_;
    QPushButton* loginButton_;
    QPushButton* signUpButton_;
    QLabel* signUpLabel_;
    QLabel* statusLabel_;

    // Server fields
    QLineEdit* hostEdit_;
    QSpinBox* portSpinBox_;

    // Saved connections
    QToolButton* savedConnectionsButton_;
    QMenu* savedConnectionsMenu_;

    // Dependencies
    ClientManager* clientManager_{nullptr};
};

}

#endif
