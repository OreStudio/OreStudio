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
#include "ores.qt/AccountDetailDialog.hpp"

#include <QtConcurrent>
#include <QFutureWatcher>
#include <QVBoxLayout>
#include <QToolBar>
#include <QIcon>
#include <QPixmap>
#include <QImage>
#include <QPainter>
#include <QMdiSubWindow>
#include <QMetaObject>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/string_generator.hpp>
#include "ui_AccountDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.iam/messaging/account_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using comms::messaging::frame;
using comms::messaging::message_type;
using namespace ores::telemetry::log;
using FutureResult = std::pair<bool, std::string>;

AccountDetailDialog::AccountDetailDialog(QWidget* parent)
    : QWidget(parent), ui_(new Ui::AccountDetailDialog), isDirty_(false),
      isAddMode_(false), isReadOnly_(false), historicalVersion_(0),
      clientManager_(nullptr), rolesWidget_(nullptr) {

    ui_->setupUi(this);

    // Create toolbar
    toolBar_ = new QToolBar(this);
    toolBar_->setMovable(false);
    toolBar_->setFloatable(false);

    const QColor iconColor(220, 220, 220);

    // Create Save action
    saveAction_ = new QAction("Save", this);
    saveAction_->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_save_20_regular.svg", iconColor));
    saveAction_->setToolTip("Save changes");
    connect(saveAction_, &QAction::triggered, this,
        &AccountDetailDialog::onSaveClicked);
    toolBar_->addAction(saveAction_);

    // Create Delete action
    deleteAction_ = new QAction("Delete", this);
    deleteAction_->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_delete_20_regular.svg", iconColor));
    deleteAction_->setToolTip("Delete account");
    connect(deleteAction_, &QAction::triggered, this,
        &AccountDetailDialog::onDeleteClicked);
    toolBar_->addAction(deleteAction_);

    // Create Revert action (initially hidden)
    revertAction_ = new QAction("Revert to this version", this);
    revertAction_->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_arrow_clockwise_16_regular.svg", iconColor));
    revertAction_->setToolTip("Revert account to this historical version");
    connect(revertAction_, &QAction::triggered, this,
        &AccountDetailDialog::onRevertClicked);
    toolBar_->addAction(revertAction_);
    revertAction_->setVisible(false);

    // Add toolbar to the dialog's layout
    auto* mainLayout = qobject_cast<QVBoxLayout*>(layout());
    if (mainLayout)
        mainLayout->insertWidget(0, toolBar_);

    // Connect signals for editable fields to detect changes
    connect(ui_->usernameEdit, &QLineEdit::textChanged, this,
        &AccountDetailDialog::onFieldChanged);
    connect(ui_->emailEdit, &QLineEdit::textChanged, this,
        &AccountDetailDialog::onFieldChanged);
    connect(ui_->passwordEdit, &QLineEdit::textChanged, this,
        &AccountDetailDialog::onFieldChanged);
    connect(ui_->confirmPasswordEdit, &QLineEdit::textChanged, this,
        &AccountDetailDialog::onFieldChanged);

    // Hide isAdminCheckBox - admin privileges are now managed via RBAC role assignments
    ui_->isAdminCheckBox->setVisible(false);

    // Create roles widget and add it to the layout before login status group
    rolesWidget_ = new AccountRolesWidget(this);
    if (mainLayout) {
        // Insert after login status group (before the spacer)
        mainLayout->insertWidget(mainLayout->count() - 1, rolesWidget_);
    }

    // Connect roles widget signals
    connect(rolesWidget_, &AccountRolesWidget::statusMessage,
            this, &AccountDetailDialog::statusMessage);
    connect(rolesWidget_, &AccountRolesWidget::errorMessage,
            this, &AccountDetailDialog::errorMessage);

    // Initially disable save/reset buttons
    updateSaveResetButtonState();
}

void AccountDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
    if (rolesWidget_) {
        rolesWidget_->setClientManager(clientManager);
    }
}

void AccountDetailDialog::setUsername(const std::string& username) {
    modifiedByUsername_ = username;
}

AccountDetailDialog::~AccountDetailDialog() {
    const auto watchers = findChildren<QFutureWatcherBase*>();
    for (auto* watcher : watchers) {
        disconnect(watcher, nullptr, this, nullptr);
        watcher->cancel();
        watcher->waitForFinished();
    }
}

void AccountDetailDialog::setAccount(const iam::domain::account& account) {
    currentAccount_ = account;
    isAddMode_ = account.username.empty();

    setCreateMode(isAddMode_);

    ui_->usernameEdit->setText(QString::fromStdString(account.username));
    ui_->emailEdit->setText(QString::fromStdString(account.email));
    ui_->versionEdit->setText(QString::number(account.version));
    ui_->modifiedByEdit->setText(QString::fromStdString(account.recorded_by));

    // Clear password fields
    ui_->passwordEdit->clear();
    ui_->confirmPasswordEdit->clear();

    // Set up roles widget for existing accounts
    if (rolesWidget_ && !isAddMode_) {
        rolesWidget_->setAccountId(account.id);
        rolesWidget_->loadRoles();
    }

    isDirty_ = false;
    emit isDirtyChanged(false);
    updateSaveResetButtonState();
}

void AccountDetailDialog::setCreateMode(bool createMode) {
    isAddMode_ = createMode;

    // Username is editable only in create mode
    ui_->usernameEdit->setReadOnly(!createMode);

    // Password section is visible only in create mode
    ui_->passwordGroup->setVisible(createMode);

    // Metadata and login status are not useful in create mode
    ui_->metadataGroup->setVisible(!createMode);
    ui_->loginStatusGroup->setVisible(!createMode);

    // Roles widget is hidden in create mode (roles are assigned after creation)
    if (rolesWidget_) {
        rolesWidget_->setVisible(!createMode);
    }
}

iam::domain::account AccountDetailDialog::getAccount() const {
    iam::domain::account account = currentAccount_;
    account.username = ui_->usernameEdit->text().toStdString();
    account.email = ui_->emailEdit->text().toStdString();
    account.recorded_by = modifiedByUsername_.empty() ? "qt_user" : modifiedByUsername_;

    return account;
}

void AccountDetailDialog::clearDialog() {
    ui_->usernameEdit->clear();
    ui_->emailEdit->clear();
    ui_->passwordEdit->clear();
    ui_->confirmPasswordEdit->clear();
    ui_->versionEdit->clear();
    ui_->modifiedByEdit->clear();

    // Clear login status fields
    ui_->onlineEdit->clear();
    ui_->lockedEdit->clear();
    ui_->failedLoginsEdit->clear();
    ui_->lastLoginEdit->clear();
    ui_->lastIpEdit->clear();
    ui_->lastAttemptIpEdit->clear();

    currentLoginInfo_.reset();
    isDirty_ = false;
    emit isDirtyChanged(false);
    updateSaveResetButtonState();
}

void AccountDetailDialog::setLoginInfo(
    const std::optional<iam::domain::login_info>& loginInfo) {
    currentLoginInfo_ = loginInfo;

    if (loginInfo.has_value()) {
        const auto& li = loginInfo.value();
        ui_->onlineEdit->setText(li.online ? tr("Yes") : tr("No"));
        ui_->lockedEdit->setText(li.locked ? tr("Yes") : tr("No"));
        ui_->failedLoginsEdit->setText(QString::number(li.failed_logins));

        // Convert time_point to string
        auto time_t_val = std::chrono::system_clock::to_time_t(li.last_login);
        if (time_t_val > 0) {
            QDateTime lastLoginDt = QDateTime::fromSecsSinceEpoch(time_t_val);
            ui_->lastLoginEdit->setText(lastLoginDt.toString(Qt::ISODate));
        } else {
            ui_->lastLoginEdit->setText(tr("Never"));
        }

        // Convert IP addresses to string
        ui_->lastIpEdit->setText(QString::fromStdString(li.last_ip.to_string()));
        ui_->lastAttemptIpEdit->setText(QString::fromStdString(li.last_attempt_ip.to_string()));
    } else {
        // No login info available
        ui_->onlineEdit->setText(tr("N/A"));
        ui_->lockedEdit->setText(tr("N/A"));
        ui_->failedLoginsEdit->setText(tr("N/A"));
        ui_->lastLoginEdit->setText(tr("N/A"));
        ui_->lastIpEdit->setText(tr("N/A"));
        ui_->lastAttemptIpEdit->setText(tr("N/A"));
    }
}

void AccountDetailDialog::save() {
    onSaveClicked();
}

bool AccountDetailDialog::validatePassword() const {
    if (!isAddMode_) {
        return true; // No password validation needed in edit mode
    }

    const QString password = ui_->passwordEdit->text();
    const QString confirmPassword = ui_->confirmPasswordEdit->text();

    if (password.isEmpty()) {
        return false;
    }

    if (password != confirmPassword) {
        return false;
    }

    // Basic password length check (server does full validation)
    if (password.length() < 12) {
        return false;
    }

    return true;
}

void AccountDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Save clicked but client not connected.";
        emit errorMessage("Not connected to server. Please login.");
        return;
    }

    // Validate username
    if (ui_->usernameEdit->text().trimmed().isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Validation failed: username is empty";
        MessageBoxHelper::warning(this, "Validation Error",
            "Username is required.");
        return;
    }

    // Validate password in create mode
    if (isAddMode_) {
        const QString password = ui_->passwordEdit->text();
        const QString confirmPassword = ui_->confirmPasswordEdit->text();

        if (password.isEmpty()) {
            BOOST_LOG_SEV(lg(), warn) << "Password validation failed: password is empty";
            MessageBoxHelper::warning(this, "Validation Error",
                "Password is required for new accounts.");
            return;
        }

        if (password != confirmPassword) {
            BOOST_LOG_SEV(lg(), warn) << "Password validation failed: passwords do not match";
            MessageBoxHelper::warning(this, "Validation Error",
                "Passwords do not match.");
            return;
        }

        if (password.length() < 12) {
            BOOST_LOG_SEV(lg(), warn) << "Password validation failed: password too short ("
                                      << password.length() << " chars, minimum 12)";
            MessageBoxHelper::warning(this, "Validation Error",
                "Password must be at least 12 characters long.");
            return;
        }
    }

    BOOST_LOG_SEV(lg(), debug) << "Save clicked for account: "
                               << currentAccount_.username;

    if (isAddMode_) {
        // Create new account
        QPointer<AccountDetailDialog> self = this;
        const std::string username = ui_->usernameEdit->text().toStdString();
        const std::string password = ui_->passwordEdit->text().toStdString();
        const std::string email = ui_->emailEdit->text().toStdString();
        const std::string recorded_by = modifiedByUsername_.empty()
            ? "qt_user" : modifiedByUsername_;

        QFuture<std::pair<bool, std::string>> future =
            QtConcurrent::run([self, username, password, email, recorded_by]()
                -> std::pair<bool, std::string> {
                if (!self) return {false, ""};

                BOOST_LOG_SEV(lg(), debug) << "Sending create account request for: "
                                           << username;

                iam::messaging::create_account_request request;
                request.username = username;
                request.password = password;
                request.email = email;
                request.recorded_by = recorded_by;
                request.totp_secret = ""; // Not used yet

                auto payload = request.serialize();
                frame request_frame(message_type::create_account_request,
                    0, std::move(payload));

                auto response_result =
                    self->clientManager_->sendRequest(std::move(request_frame));

                if (!response_result) {
                    return {false, "Failed to communicate with server"};
                }

                auto payload_result = response_result->decompressed_payload();
                if (!payload_result) {
                    return {false, "Failed to decompress server response"};
                }

                auto response = iam::messaging::create_account_response::
                    deserialize(*payload_result);

                if (!response) {
                    return {false, "Invalid server response"};
                }

                // Success if we got an account_id back
                return {true, boost::uuids::to_string(response->account_id)};
            });

        auto* watcher = new QFutureWatcher<std::pair<bool, std::string>>(self);
        connect(watcher, &QFutureWatcher<std::pair<bool, std::string>>::finished, self,
            [self, watcher, username]() {
            if (!self) return;

            auto [success, message] = watcher->result();
            watcher->deleteLater();

            if (success) {
                BOOST_LOG_SEV(lg(), debug) << "Account created successfully: "
                                           << message;

                emit self->statusMessage(QString("Successfully created account: %1")
                    .arg(QString::fromStdString(username)));

                self->isDirty_ = false;
                emit self->isDirtyChanged(false);
                self->updateSaveResetButtonState();

                // Parse UUID from message and emit signal
                boost::uuids::uuid account_id;
                try {
                    boost::uuids::string_generator gen;
                    account_id = gen(message);
                    emit self->accountCreated(account_id);
                } catch (...) {
                    BOOST_LOG_SEV(lg(), warn) << "Failed to parse account ID";
                }

                // Close window after successful creation
                QWidget* parent = self->parentWidget();
                while (parent) {
                    if (auto* mdiSubWindow = qobject_cast<QMdiSubWindow*>(parent)) {
                        QMetaObject::invokeMethod(mdiSubWindow, "close",
                            Qt::QueuedConnection);
                        break;
                    }
                    parent = parent->parentWidget();
                }
            } else {
                BOOST_LOG_SEV(lg(), error) << "Account creation failed: " << message;
                emit self->errorMessage(QString("Failed to create account: %1")
                    .arg(QString::fromStdString(message)));
                MessageBoxHelper::critical(self, "Create Failed",
                    QString::fromStdString(message));
            }
        });

        watcher->setFuture(future);
    } else {
        // Edit mode - update existing account
        QPointer<AccountDetailDialog> self = this;
        const boost::uuids::uuid account_id = currentAccount_.id;
        const std::string email = ui_->emailEdit->text().toStdString();
        const std::string recorded_by = modifiedByUsername_.empty()
            ? "qt_user" : modifiedByUsername_;

        QFuture<std::pair<bool, std::string>> future =
            QtConcurrent::run([self, account_id, email, recorded_by]()
                -> std::pair<bool, std::string> {
                if (!self) return {false, ""};

                BOOST_LOG_SEV(lg(), debug) << "Sending update account request for: "
                                           << boost::uuids::to_string(account_id);

                iam::messaging::update_account_request request;
                request.account_id = account_id;
                request.email = email;
                request.recorded_by = recorded_by;

                auto payload = request.serialize();
                frame request_frame(message_type::update_account_request,
                    0, std::move(payload));

                auto response_result =
                    self->clientManager_->sendRequest(std::move(request_frame));

                if (!response_result) {
                    return {false, "Failed to communicate with server"};
                }

                auto payload_result = response_result->decompressed_payload();
                if (!payload_result) {
                    return {false, "Failed to decompress server response"};
                }

                auto response = iam::messaging::update_account_response::
                    deserialize(*payload_result);

                if (!response) {
                    return {false, "Invalid server response"};
                }

                return {response->success, response->error_message};
            });

        auto* watcher = new QFutureWatcher<std::pair<bool, std::string>>(self);
        connect(watcher, &QFutureWatcher<std::pair<bool, std::string>>::finished, self,
            [self, watcher, account_id]() {
            if (!self) return;

            auto [success, message] = watcher->result();
            watcher->deleteLater();

            if (success) {
                BOOST_LOG_SEV(lg(), debug) << "Account updated successfully";

                emit self->statusMessage(QString("Successfully updated account"));

                self->isDirty_ = false;
                emit self->isDirtyChanged(false);
                self->updateSaveResetButtonState();

                emit self->accountUpdated(account_id);

                // Close window after successful update
                QWidget* parent = self->parentWidget();
                while (parent) {
                    if (auto* mdiSubWindow = qobject_cast<QMdiSubWindow*>(parent)) {
                        QMetaObject::invokeMethod(mdiSubWindow, "close",
                            Qt::QueuedConnection);
                        break;
                    }
                    parent = parent->parentWidget();
                }
            } else {
                BOOST_LOG_SEV(lg(), error) << "Account update failed: " << message;
                emit self->errorMessage(QString("Failed to update account: %1")
                    .arg(QString::fromStdString(message)));
                MessageBoxHelper::critical(self, "Update Failed",
                    QString::fromStdString(message));
            }
        });

        watcher->setFuture(future);
    }
}

void AccountDetailDialog::onResetClicked() {
    BOOST_LOG_SEV(lg(), debug) << "Reset clicked for account: "
                               << currentAccount_.username;
    setAccount(currentAccount_);
}

void AccountDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Delete clicked but client not connected.";
        emit errorMessage("Not connected to server. Please login.");
        return;
    }

    if (isAddMode_) {
        BOOST_LOG_SEV(lg(), warn) << "Delete clicked in add mode.";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Delete request for account: "
                               << currentAccount_.username;

    auto reply = MessageBoxHelper::question(this, "Delete Account",
        QString("Are you sure you want to delete account '%1'?")
            .arg(QString::fromStdString(currentAccount_.username)),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        BOOST_LOG_SEV(lg(), debug) << "Delete cancelled by user";
        return;
    }

    QPointer<AccountDetailDialog> self = this;
    const boost::uuids::uuid account_id = currentAccount_.id;
    const std::string username = currentAccount_.username;

    QFuture<FutureResult> future =
        QtConcurrent::run([self, account_id]() -> FutureResult {
            if (!self) return {false, {}};

            BOOST_LOG_SEV(lg(), debug) << "Sending delete account request for: "
                                       << boost::uuids::to_string(account_id);

            iam::messaging::delete_account_request request{account_id};
            auto payload = request.serialize();

            frame request_frame(message_type::delete_account_request,
                0, std::move(payload));

            auto response_result =
                self->clientManager_->sendRequest(std::move(request_frame));

            if (!response_result) {
                return {false, "Failed to communicate with server"};
            }

            auto payload_result = response_result->decompressed_payload();
            if (!payload_result) {
                return {false, "Failed to decompress server response"};
            }

            auto response = iam::messaging::delete_account_response::
                deserialize(*payload_result);

            if (!response) {
                return {false, "Invalid server response"};
            }

            return {response->success, response->message};
        });

    auto* watcher = new QFutureWatcher<FutureResult>(self);
    connect(watcher, &QFutureWatcher<FutureResult>::finished, self,
        [self, watcher, account_id, username]() {

        if (!self) return;

        auto [success, message] = watcher->result();
        watcher->deleteLater();

        if (success) {
            BOOST_LOG_SEV(lg(), debug) << "Account deleted successfully.";
            emit self->statusMessage(QString("Successfully deleted account: %1")
                .arg(QString::fromStdString(username)));
            emit self->accountDeleted(account_id);

            // Close window after successful deletion
            QWidget* parent = self->parentWidget();
            while (parent) {
                if (auto* mdiSubWindow = qobject_cast<QMdiSubWindow*>(parent)) {
                    QMetaObject::invokeMethod(mdiSubWindow, "close",
                        Qt::QueuedConnection);
                    break;
                }
                parent = parent->parentWidget();
            }
        } else {
            BOOST_LOG_SEV(lg(), error) << "Account deletion failed: " << message;
            emit self->errorMessage(QString("Failed to delete account: %1")
                .arg(QString::fromStdString(message)));
            MessageBoxHelper::critical(self, "Delete Failed",
                QString::fromStdString(message));
        }
    });

    watcher->setFuture(future);
}

void AccountDetailDialog::onFieldChanged() {
    if (isReadOnly_)
        return;

    isDirty_ = true;
    emit isDirtyChanged(true);
    updateSaveResetButtonState();
}

void AccountDetailDialog::onRevertClicked() {
    BOOST_LOG_SEV(lg(), info) << "Revert clicked for historical version "
                              << historicalVersion_;

    // Confirm with user
    auto reply = MessageBoxHelper::question(this, "Revert Account",
        QString("Are you sure you want to revert '%1' to version %2?\n\n"
                "This will create a new version with the data from version %2.")
            .arg(QString::fromStdString(currentAccount_.username))
            .arg(historicalVersion_),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        BOOST_LOG_SEV(lg(), debug) << "Revert cancelled by user";
        return;
    }

    emit revertRequested(currentAccount_);
}

void AccountDetailDialog::setReadOnly(bool readOnly, int versionNumber) {
    isReadOnly_ = readOnly;
    historicalVersion_ = versionNumber;

    BOOST_LOG_SEV(lg(), debug) << "Setting read-only mode: " << readOnly
                               << ", version: " << versionNumber;

    setFieldsReadOnly(readOnly);

    // Update toolbar visibility
    if (saveAction_)
        saveAction_->setVisible(!readOnly);

    if (deleteAction_)
        deleteAction_->setVisible(!readOnly);

    if (revertAction_)
        revertAction_->setVisible(readOnly);

    // Hide password group in read-only mode
    ui_->passwordGroup->setVisible(false);

    // Set roles widget to read-only or hide it for historical versions
    if (rolesWidget_) {
        rolesWidget_->setReadOnly(readOnly);
        // Hide roles widget for historical versions (they don't have role data)
        rolesWidget_->setVisible(versionNumber == 0);
    }

    updateSaveResetButtonState();
}

void AccountDetailDialog::setFieldsReadOnly(bool readOnly) {
    ui_->usernameEdit->setReadOnly(readOnly);
    ui_->emailEdit->setReadOnly(readOnly);
    // Note: isAdminCheckBox is hidden - admin privileges managed via RBAC
}

void AccountDetailDialog::updateSaveResetButtonState() {
    if (isReadOnly_) {
        if (saveAction_)
            saveAction_->setEnabled(false);
        if (deleteAction_)
            deleteAction_->setEnabled(false);
        if (revertAction_)
            revertAction_->setEnabled(true);
        return;
    }

    if (saveAction_)
        saveAction_->setEnabled(isDirty_);

    if (deleteAction_)
        deleteAction_->setEnabled(!isAddMode_);
}

}
