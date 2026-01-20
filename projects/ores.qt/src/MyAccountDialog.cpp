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
#include "ores.qt/MyAccountDialog.hpp"

#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QFormLayout>
#include <QtConcurrent>
#include <QFutureWatcher>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/SessionHistoryDialog.hpp"
#include "ores.comms/messaging/frame.hpp"
#include "ores.comms/messaging/message_types.hpp"
#include "ores.iam/messaging/protocol.hpp"
#include "ores.iam/domain/session.hpp"

namespace ores::qt {

using namespace ores::logging;

MyAccountDialog::MyAccountDialog(ClientManager* clientManager, QWidget* parent)
    : QDialog(parent),
      username_edit_(new QLineEdit(this)),
      email_edit_(new QLineEdit(this)),
      save_email_button_(new QPushButton("Save", this)),
      email_status_label_(new QLabel(this)),
      sessions_group_(new QGroupBox("Sessions", this)),
      active_sessions_label_(new QLabel(this)),
      current_session_label_(new QLabel(this)),
      view_sessions_button_(new QPushButton("View Session History", this)),
      password_group_(new QGroupBox("Change Password", this)),
      new_password_edit_(new QLineEdit(this)),
      confirm_password_edit_(new QLineEdit(this)),
      change_password_button_(new QPushButton("Change Password", this)),
      password_status_label_(new QLabel(this)),
      close_button_(new QPushButton("Close", this)),
      clientManager_(clientManager) {

    setupUI();
    loadAccountInfo();
    loadSessionInfo();

    // Connect signals
    connect(change_password_button_, &QPushButton::clicked,
            this, &MyAccountDialog::onChangePasswordClicked);
    connect(save_email_button_, &QPushButton::clicked,
            this, &MyAccountDialog::onSaveEmailClicked);
    connect(view_sessions_button_, &QPushButton::clicked,
            this, &MyAccountDialog::onViewSessionsClicked);
    connect(close_button_, &QPushButton::clicked,
            this, &MyAccountDialog::onCloseClicked);
    connect(this, &MyAccountDialog::changePasswordCompleted,
            this, &MyAccountDialog::onChangePasswordResult);
    connect(this, &MyAccountDialog::saveEmailCompleted,
            this, &MyAccountDialog::onSaveEmailResult);
}

MyAccountDialog::~MyAccountDialog() {
}

void MyAccountDialog::setupUI() {
    BOOST_LOG_SEV(lg(), debug) << "Setting up UI.";

    setWindowTitle("My Account");
    setModal(true);
    setMinimumWidth(450);
    setFixedWidth(450);
    setSizeGripEnabled(false);

    const QColor iconColor(220, 220, 220);

    // Account Info Group
    auto* account_group = new QGroupBox("Account Information", this);
    auto* account_layout = new QFormLayout(account_group);
    account_layout->setFieldGrowthPolicy(QFormLayout::ExpandingFieldsGrow);

    // Username (read-only)
    username_edit_->setReadOnly(true);
    username_edit_->setStyleSheet("QLineEdit { background-color: #3a3a3a; }");
    account_layout->addRow("Username:", username_edit_);

    // Email field with save button
    auto* email_layout = new QHBoxLayout();
    email_edit_->setPlaceholderText("Enter email address");
    email_layout->addWidget(email_edit_);
    save_email_button_->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    save_email_button_->setFixedWidth(80);
    email_layout->addWidget(save_email_button_);
    account_layout->addRow("Email:", email_layout);

    // Email status label
    email_status_label_->setWordWrap(true);
    email_status_label_->setStyleSheet("QLabel { color: #666; font-style: italic; }");
    account_layout->addRow("", email_status_label_);

    // Sessions Group
    auto* sessions_layout = new QFormLayout(sessions_group_);
    sessions_layout->setFieldGrowthPolicy(QFormLayout::ExpandingFieldsGrow);

    active_sessions_label_->setText("Loading...");
    active_sessions_label_->setStyleSheet("QLabel { font-weight: bold; }");
    sessions_layout->addRow("Active Sessions:", active_sessions_label_);

    current_session_label_->setText("");
    current_session_label_->setStyleSheet("QLabel { color: #666; font-style: italic; }");
    sessions_layout->addRow("", current_session_label_);

    view_sessions_button_->setIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));
    auto* sessions_button_layout = new QHBoxLayout();
    sessions_button_layout->addStretch();
    sessions_button_layout->addWidget(view_sessions_button_);
    sessions_layout->addRow(sessions_button_layout);

    // Password Change Group
    auto* password_layout = new QFormLayout(password_group_);
    password_layout->setFieldGrowthPolicy(QFormLayout::ExpandingFieldsGrow);

    // New password field
    new_password_edit_->setEchoMode(QLineEdit::Password);
    new_password_edit_->setPlaceholderText("Enter new password");
    password_layout->addRow("New Password:", new_password_edit_);

    // Confirm password field
    confirm_password_edit_->setEchoMode(QLineEdit::Password);
    confirm_password_edit_->setPlaceholderText("Confirm new password");
    password_layout->addRow("Confirm Password:", confirm_password_edit_);

    // Password status label
    password_status_label_->setWordWrap(true);
    password_status_label_->setStyleSheet("QLabel { color: #666; font-style: italic; }");

    // Change password button
    change_password_button_->setIcon(
        IconUtils::createRecoloredIcon(Icon::Key, IconUtils::DefaultIconColor));

    auto* password_button_layout = new QHBoxLayout();
    password_button_layout->addWidget(password_status_label_);
    password_button_layout->addStretch();
    password_button_layout->addWidget(change_password_button_);

    password_layout->addRow(password_button_layout);

    // Close button
    close_button_->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));

    auto* button_layout = new QHBoxLayout();
    button_layout->addStretch();
    button_layout->addWidget(close_button_);

    // Main layout
    auto* main_layout = new QVBoxLayout(this);
    main_layout->addWidget(account_group);
    main_layout->addWidget(sessions_group_);
    main_layout->addWidget(password_group_);
    main_layout->addSpacing(10);
    main_layout->addLayout(button_layout);

    // Set default button
    close_button_->setDefault(true);

    // Connect password fields for match indicator
    connect(new_password_edit_, &QLineEdit::textChanged, this,
        &MyAccountDialog::updatePasswordMatchIndicator);
    connect(confirm_password_edit_, &QLineEdit::textChanged, this,
        &MyAccountDialog::updatePasswordMatchIndicator);
}

void MyAccountDialog::loadAccountInfo() {
    BOOST_LOG_SEV(lg(), debug) << "Loading account info.";

    if (!clientManager_) {
        BOOST_LOG_SEV(lg(), error) << "Client manager not available.";
        return;
    }

    // Get current user info from client manager
    const auto& username = clientManager_->currentUsername();
    username_edit_->setText(QString::fromStdString(username));

    // Get current email from client manager
    const auto& email = clientManager_->currentEmail();
    email_edit_->setText(QString::fromStdString(email));

    BOOST_LOG_SEV(lg(), debug) << "Loaded account info for user: " << username;
}

void MyAccountDialog::enablePasswordForm(bool enabled) {
    BOOST_LOG_SEV(lg(), trace) << "Enable password form: " << enabled;

    new_password_edit_->setEnabled(enabled);
    confirm_password_edit_->setEnabled(enabled);
    change_password_button_->setEnabled(enabled);
}

bool MyAccountDialog::validatePasswordInput() {
    const auto new_password = new_password_edit_->text();
    const auto confirm_password = confirm_password_edit_->text();

    if (new_password.isEmpty()) {
        MessageBoxHelper::warning(this, "Invalid Input", "Please enter a new password.");
        new_password_edit_->setFocus();
        return false;
    }

    if (confirm_password.isEmpty()) {
        MessageBoxHelper::warning(this, "Invalid Input", "Please confirm your new password.");
        confirm_password_edit_->setFocus();
        return false;
    }

    if (new_password != confirm_password) {
        MessageBoxHelper::warning(this, "Password Mismatch",
            "The passwords do not match. Please try again.");
        confirm_password_edit_->clear();
        confirm_password_edit_->setFocus();
        return false;
    }

    // Basic password strength check
    if (new_password.length() < 8) {
        MessageBoxHelper::warning(this, "Weak Password",
            "Password must be at least 8 characters long.");
        new_password_edit_->setFocus();
        return false;
    }

    return true;
}

void MyAccountDialog::onChangePasswordClicked() {
    if (!clientManager_) {
        MessageBoxHelper::critical(this, "Internal Error", "Client manager not initialized");
        return;
    }

    const auto& username = clientManager_->currentUsername();
    BOOST_LOG_SEV(lg(), info)
        << "Change password: user '" << username
        << "' initiating voluntary password change via My Account";

    if (!validatePasswordInput()) {
        BOOST_LOG_SEV(lg(), debug)
            << "Change password: validation failed for user '" << username << "'";
        return;
    }

    const auto new_password = new_password_edit_->text();

    // Disable form during request
    enablePasswordForm(false);
    password_status_label_->setText("Changing password...");
    password_status_label_->setStyleSheet("QLabel { color: #666; font-style: italic; }");

    // Perform password change asynchronously
    auto* watcher = new QFutureWatcher<std::pair<bool, QString>>(this);
    connect(watcher, &QFutureWatcher<std::pair<bool, QString>>::finished,
            [this, watcher]() {
        const auto [success, error_msg] = watcher->result();
        watcher->deleteLater();
        emit changePasswordCompleted(success, error_msg);
    });

    QFuture<std::pair<bool, QString>> future = QtConcurrent::run(
        [this, new_password]() -> std::pair<bool, QString> {
            try {
                iam::messaging::change_password_request request{
                    .new_password = new_password.toStdString()
                };

                auto payload = request.serialize();
                comms::messaging::frame request_frame(
                    comms::messaging::message_type::change_password_request,
                    0,
                    std::move(payload)
                );

                auto response_result = clientManager_->sendRequest(std::move(request_frame));

                if (!response_result) {
                    return {false, QString("Network error during password change")};
                }

                const auto& header = response_result->header();

                // Check for error response
                if (header.type == comms::messaging::message_type::error_response) {
                    auto payload_result = response_result->decompressed_payload();
                    if (payload_result) {
                        auto error_resp = comms::messaging::error_response::deserialize(*payload_result);
                        if (error_resp) {
                            return {false, QString::fromStdString(error_resp->message)};
                        }
                    }
                    return {false, QString("Unknown server error")};
                }

                // Decompress payload
                auto payload_result = response_result->decompressed_payload();
                if (!payload_result) {
                    return {false, QString("Failed to decompress server response")};
                }

                auto response = iam::messaging::change_password_response::deserialize(*payload_result);

                if (!response) {
                    return {false, QString("Invalid response from server")};
                }

                if (!response->success) {
                    return {false, QString::fromStdString(response->message)};
                }

                return {true, QString()};
            } catch (const std::exception& e) {
                return {false, QString::fromStdString(e.what())};
            }
        }
    );

    watcher->setFuture(future);
}

void MyAccountDialog::onChangePasswordResult(bool success, const QString& error_message) {
    const auto& username = clientManager_ ? clientManager_->currentUsername() : "<unknown>";

    enablePasswordForm(true);

    if (success) {
        BOOST_LOG_SEV(lg(), info)
            << "Change password: user '" << username
            << "' completed voluntary password change via My Account";
        password_status_label_->setText("Password changed successfully!");
        password_status_label_->setStyleSheet("QLabel { color: #0a0; }");

        // Clear the password fields
        new_password_edit_->clear();
        confirm_password_edit_->clear();

        MessageBoxHelper::information(this, "Success",
            "Your password has been changed successfully.");
    } else {
        BOOST_LOG_SEV(lg(), warn)
            << "Change password failed: user '" << username
            << "' voluntary password change failed - " << error_message.toStdString();

        password_status_label_->setText("");

        MessageBoxHelper::critical(this, "Password Change Failed",
            QString("Failed to change password: %1").arg(error_message));
    }
}

void MyAccountDialog::onSaveEmailClicked() {
    if (!clientManager_) {
        MessageBoxHelper::critical(this, "Internal Error", "Client manager not initialized");
        return;
    }

    const auto& username = clientManager_->currentUsername();
    const auto new_email = email_edit_->text().trimmed();

    BOOST_LOG_SEV(lg(), info)
        << "Update email: user '" << username
        << "' initiating email update via My Account";

    if (new_email.isEmpty()) {
        MessageBoxHelper::warning(this, "Invalid Input", "Please enter an email address.");
        email_edit_->setFocus();
        return;
    }

    if (!new_email.contains('@')) {
        MessageBoxHelper::warning(this, "Invalid Input", "Please enter a valid email address.");
        email_edit_->setFocus();
        return;
    }

    // Disable form during request
    email_edit_->setEnabled(false);
    save_email_button_->setEnabled(false);
    email_status_label_->setText("Saving email...");
    email_status_label_->setStyleSheet("QLabel { color: #666; font-style: italic; }");

    // Perform email update asynchronously
    auto* watcher = new QFutureWatcher<std::pair<bool, QString>>(this);
    connect(watcher, &QFutureWatcher<std::pair<bool, QString>>::finished,
            [this, watcher]() {
        const auto [success, error_msg] = watcher->result();
        watcher->deleteLater();
        emit saveEmailCompleted(success, error_msg);
    });

    QFuture<std::pair<bool, QString>> future = QtConcurrent::run(
        [this, new_email]() -> std::pair<bool, QString> {
            try {
                iam::messaging::update_my_email_request request{
                    .new_email = new_email.toStdString()
                };

                auto payload = request.serialize();
                comms::messaging::frame request_frame(
                    comms::messaging::message_type::update_my_email_request,
                    0,
                    std::move(payload)
                );

                auto response_result = clientManager_->sendRequest(std::move(request_frame));

                if (!response_result) {
                    return {false, QString("Network error during email update")};
                }

                const auto& header = response_result->header();

                // Check for error response
                if (header.type == comms::messaging::message_type::error_response) {
                    auto payload_result = response_result->decompressed_payload();
                    if (payload_result) {
                        auto error_resp = comms::messaging::error_response::deserialize(*payload_result);
                        if (error_resp) {
                            return {false, QString::fromStdString(error_resp->message)};
                        }
                    }
                    return {false, QString("Unknown server error")};
                }

                // Decompress payload
                auto payload_result = response_result->decompressed_payload();
                if (!payload_result) {
                    return {false, QString("Failed to decompress server response")};
                }

                auto response = iam::messaging::update_my_email_response::deserialize(*payload_result);

                if (!response) {
                    return {false, QString("Invalid response from server")};
                }

                if (!response->success) {
                    return {false, QString::fromStdString(response->message)};
                }

                // Update the stored email in ClientManager
                clientManager_->setCurrentEmail(new_email.toStdString());

                return {true, QString()};
            } catch (const std::exception& e) {
                return {false, QString::fromStdString(e.what())};
            }
        }
    );

    watcher->setFuture(future);
}

void MyAccountDialog::onSaveEmailResult(bool success, const QString& error_message) {
    const auto& username = clientManager_ ? clientManager_->currentUsername() : "<unknown>";

    email_edit_->setEnabled(true);
    save_email_button_->setEnabled(true);

    if (success) {
        BOOST_LOG_SEV(lg(), info)
            << "Update email: user '" << username
            << "' successfully updated their email via My Account";
        email_status_label_->setText("Email saved successfully!");
        email_status_label_->setStyleSheet("QLabel { color: #0a0; }");
    } else {
        BOOST_LOG_SEV(lg(), warn)
            << "Update email failed: user '" << username
            << "' email update failed - " << error_message.toStdString();

        email_status_label_->setText("");

        MessageBoxHelper::critical(this, "Email Update Failed",
            QString("Failed to update email: %1").arg(error_message));
    }
}

void MyAccountDialog::onCloseClicked() {
    BOOST_LOG_SEV(lg(), trace) << "On close clicked.";
    accept();
}

void MyAccountDialog::loadSessionInfo() {
    BOOST_LOG_SEV(lg(), debug) << "Loading session info.";

    if (!clientManager_) {
        BOOST_LOG_SEV(lg(), error) << "Client manager not available.";
        active_sessions_label_->setText("N/A");
        return;
    }

    // Load session info asynchronously
    auto* watcher = new QFutureWatcher<std::optional<std::vector<iam::domain::session>>>(this);
    connect(watcher, &QFutureWatcher<std::optional<std::vector<iam::domain::session>>>::finished,
            [this, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result) {
            const auto& sessions = *result;
            active_sessions_label_->setText(QString::number(sessions.size()));

            if (!sessions.empty()) {
                // Show info about the current session (first active session)
                const auto& current = sessions.front();
                QString info = QString("Current: %1 (%2)")
                    .arg(QString::fromStdString(current.client_identifier.empty()
                        ? "Unknown Client" : current.client_identifier))
                    .arg(QString::fromStdString(current.client_ip.to_string()));
                current_session_label_->setText(info);
            }
        } else {
            active_sessions_label_->setText("Error loading");
            active_sessions_label_->setStyleSheet("QLabel { color: #f00; font-weight: bold; }");
        }
    });

    QFuture<std::optional<std::vector<iam::domain::session>>> future = QtConcurrent::run(
        [this]() -> std::optional<std::vector<iam::domain::session>> {
            return clientManager_->getActiveSessions();
        }
    );

    watcher->setFuture(future);
}

void MyAccountDialog::onViewSessionsClicked() {
    BOOST_LOG_SEV(lg(), debug) << "View sessions clicked.";

    if (!clientManager_) {
        MessageBoxHelper::critical(this, "Internal Error", "Client manager not initialized");
        return;
    }

    auto accountId = clientManager_->accountId();
    if (!accountId) {
        MessageBoxHelper::warning(this, "Not Logged In", "Please log in to view session history.");
        return;
    }

    // Open session history dialog
    auto* sessionDialog = new SessionHistoryDialog(clientManager_, this);
    sessionDialog->setAccount(*accountId, QString::fromStdString(clientManager_->currentUsername()));
    sessionDialog->setAttribute(Qt::WA_DeleteOnClose);
    sessionDialog->setModal(false);
    sessionDialog->show();
}

void MyAccountDialog::updatePasswordMatchIndicator() {
    const QString password = new_password_edit_->text();
    const QString confirmPassword = confirm_password_edit_->text();

    // Only show indicator when confirm field has content
    if (confirmPassword.isEmpty()) {
        // Reset to default style
        confirm_password_edit_->setStyleSheet("");
        return;
    }

    if (password == confirmPassword) {
        // Green border for matching passwords
        confirm_password_edit_->setStyleSheet(
            "QLineEdit { border: 2px solid #4CAF50; }");
    } else {
        // Orange/red border for non-matching passwords
        confirm_password_edit_->setStyleSheet(
            "QLineEdit { border: 2px solid #FF9800; }");
    }
}

}
