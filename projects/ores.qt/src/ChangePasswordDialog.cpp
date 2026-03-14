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
#include "ores.qt/ChangePasswordDialog.hpp"
#include "ores.qt/PasswordMatchIndicator.hpp"

#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QFormLayout>
#include <QtConcurrent>
#include <QFutureWatcher>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.iam/messaging/account_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

ChangePasswordDialog::ChangePasswordDialog(ClientManager* clientManager, QWidget* parent)
    : QDialog(parent),
      new_password_edit_(new QLineEdit(this)),
      confirm_password_edit_(new QLineEdit(this)),
      change_button_(new QPushButton("Change Password", this)),
      cancel_button_(new QPushButton("Cancel", this)),
      status_label_(new QLabel(this)),
      info_label_(new QLabel(this)),
      clientManager_(clientManager) {

    setupUI();

    // Connect signals
    connect(change_button_, &QPushButton::clicked, this, &ChangePasswordDialog::onChangeClicked);
    connect(cancel_button_, &QPushButton::clicked, this, &QDialog::reject);
    connect(this, &ChangePasswordDialog::changeCompleted,
            this, &ChangePasswordDialog::onChangeResult);
}

ChangePasswordDialog::~ChangePasswordDialog() {
}

void ChangePasswordDialog::setupUI() {
    BOOST_LOG_SEV(lg(), debug) << "Setting up UI.";

    setWindowTitle("Change Password");
    setModal(true);
    setMinimumWidth(400);
    setFixedWidth(400);
    setSizeGripEnabled(false);

    // Info label explaining why password change is required
    info_label_->setText("Your administrator has required you to change your password.\n"
                         "Please enter a new password below.");
    info_label_->setWordWrap(true);
    info_label_->setStyleSheet("QLabel { color: #f80; margin-bottom: 10px; }");

    // Create form layout
    auto* form_layout = new QFormLayout();
    form_layout->setFieldGrowthPolicy(QFormLayout::ExpandingFieldsGrow);

    // New password field
    new_password_edit_->setEchoMode(QLineEdit::Password);
    new_password_edit_->setPlaceholderText("Enter new password");
    form_layout->addRow("New Password:", new_password_edit_);

    // Confirm password field
    confirm_password_edit_->setEchoMode(QLineEdit::Password);
    confirm_password_edit_->setPlaceholderText("Confirm new password");
    form_layout->addRow("Confirm Password:", confirm_password_edit_);

    // Status label
    status_label_->setWordWrap(true);
    status_label_->setStyleSheet("QLabel { color: #666; font-style: italic; }");

    // Set icons on buttons
    change_button_->setIcon(IconUtils::createRecoloredIcon(Icon::Checkmark, IconUtils::DefaultIconColor));
    cancel_button_->setIcon(IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));

    // Button layout
    auto* button_layout = new QHBoxLayout();
    button_layout->addStretch();
    button_layout->addWidget(cancel_button_);
    button_layout->addWidget(change_button_);

    // Main layout
    auto* main_layout = new QVBoxLayout(this);
    main_layout->addWidget(info_label_);
    main_layout->addLayout(form_layout);
    main_layout->addWidget(status_label_);
    main_layout->addSpacing(10);
    main_layout->addLayout(button_layout);

    // Set default button
    change_button_->setDefault(true);

    // Focus on new password field
    new_password_edit_->setFocus();

    // Connect password fields for match indicator
    PasswordMatchIndicator::connectFields(
        new_password_edit_, confirm_password_edit_);
}

void ChangePasswordDialog::enableForm(bool enabled) {
    BOOST_LOG_SEV(lg(), trace) << "Enable form: " << enabled;

    new_password_edit_->setEnabled(enabled);
    confirm_password_edit_->setEnabled(enabled);
    change_button_->setEnabled(enabled);
}

bool ChangePasswordDialog::validateInput() {
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
        MessageBoxHelper::warning(this, "Password Mismatch", "The passwords do not match. Please try again.");
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

void ChangePasswordDialog::onChangeClicked() {
    if (!clientManager_) {
        MessageBoxHelper::critical(this, "Internal Error", "Client manager not initialized");
        return;
    }

    const auto& username = clientManager_->currentUsername();
    BOOST_LOG_SEV(lg(), info)
        << "Change password: user '" << username
        << "' initiating forced password change (admin-required reset)";

    if (!validateInput()) {
        BOOST_LOG_SEV(lg(), debug) << "Change password: validation failed for user '"
                                   << username << "'";
        return;
    }

    const auto new_password = new_password_edit_->text();

    // Disable form during request
    enableForm(false);
    status_label_->setText("Changing password...");
    status_label_->setStyleSheet("QLabel { color: #666; font-style: italic; }");

    // Perform password change asynchronously
    auto* watcher = new QFutureWatcher<std::pair<bool, QString>>(this);
    connect(watcher, &QFutureWatcher<std::pair<bool, QString>>::finished,
            [this, watcher]() {
        const auto [success, error_msg] = watcher->result();
        watcher->deleteLater();
        emit changeCompleted(success, error_msg);
    });

    QFuture<std::pair<bool, QString>> future = QtConcurrent::run(
        [this, new_password]() -> std::pair<bool, QString> {
            try {
                iam::messaging::change_password_request_typed request{
                    .new_password = new_password.toStdString()
                };

                auto result = clientManager_->process_authenticated_request(
                    std::move(request));

                if (!result) {
                    return {false, QString::fromStdString(result.error())};
                }

                if (!result->success) {
                    return {false, QString::fromStdString(result->message)};
                }

                return {true, QString()};
            } catch (const std::exception& e) {
                return {false, QString::fromStdString(e.what())};
            }
        }
    );

    watcher->setFuture(future);
}

void ChangePasswordDialog::onChangeResult(bool success, const QString& error_message) {
    const auto& username = clientManager_ ? clientManager_->currentUsername() : "<unknown>";

    if (success) {
        BOOST_LOG_SEV(lg(), info)
            << "Change password: user '" << username
            << "' completed forced password change";
        status_label_->setText("Password changed successfully!");
        status_label_->setStyleSheet("QLabel { color: #0a0; }");
        accept();  // Close dialog with success
    } else {
        BOOST_LOG_SEV(lg(), warn)
            << "Change password failed: user '" << username
            << "' forced password change failed - " << error_message.toStdString();

        enableForm(true);
        status_label_->setText("");

        MessageBoxHelper::critical(this, "Password Change Failed",
            QString("Failed to change password: %1").arg(error_message));
    }
}

}
