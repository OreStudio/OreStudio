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
#include "ores.qt/MasterPasswordDialog.hpp"
#include <QVBoxLayout>
#include <QFormLayout>
#include <QMessageBox>

namespace ores::qt {

MasterPasswordDialog::MasterPasswordDialog(Mode mode, QWidget* parent)
    : QDialog(parent),
      mode_(mode) {

    setupUI();
}

MasterPasswordDialog::~MasterPasswordDialog() = default;

void MasterPasswordDialog::setupUI() {
    using namespace ores::logging;

    auto* layout = new QVBoxLayout(this);

    infoLabel_ = new QLabel(this);
    infoLabel_->setWordWrap(true);
    layout->addWidget(infoLabel_);

    auto* formLayout = new QFormLayout();

    currentPasswordEdit_ = new QLineEdit(this);
    currentPasswordEdit_->setEchoMode(QLineEdit::Password);

    newPasswordEdit_ = new QLineEdit(this);
    newPasswordEdit_->setEchoMode(QLineEdit::Password);

    confirmPasswordEdit_ = new QLineEdit(this);
    confirmPasswordEdit_->setEchoMode(QLineEdit::Password);

    matchIndicatorLabel_ = new QLabel(this);
    matchIndicatorLabel_->setStyleSheet("color: red;");

    switch (mode_) {
    case Mode::Unlock:
        setWindowTitle(tr("Unlock Connections"));
        infoLabel_->setText(tr("Enter the master password to access your saved connections."));
        formLayout->addRow(tr("Password:"), currentPasswordEdit_);
        newPasswordEdit_->hide();
        confirmPasswordEdit_->hide();
        matchIndicatorLabel_->hide();
        break;

    case Mode::Create:
        setWindowTitle(tr("Create Master Password"));
        infoLabel_->setText(tr(
            "Create a master password to protect your saved connection credentials.\n\n"
            "This password will be used to encrypt your server passwords. "
            "Make sure you remember it - there is no way to recover encrypted passwords "
            "without it.\n\n"
            "You may leave this blank if you don't want password protection, "
            "but your saved passwords will not be encrypted."));
        currentPasswordEdit_->hide();
        formLayout->addRow(tr("New Password:"), newPasswordEdit_);
        formLayout->addRow(tr("Confirm Password:"), confirmPasswordEdit_);
        formLayout->addRow(QString(), matchIndicatorLabel_);
        break;

    case Mode::Change:
        setWindowTitle(tr("Change Master Password"));
        infoLabel_->setText(tr(
            "Enter your current password and a new password.\n\n"
            "All stored connection passwords will be re-encrypted with the new password."));
        formLayout->addRow(tr("Current Password:"), currentPasswordEdit_);
        formLayout->addRow(tr("New Password:"), newPasswordEdit_);
        formLayout->addRow(tr("Confirm New Password:"), confirmPasswordEdit_);
        formLayout->addRow(QString(), matchIndicatorLabel_);
        break;
    }

    layout->addLayout(formLayout);

    buttonBox_ = new QDialogButtonBox(
        QDialogButtonBox::Ok | QDialogButtonBox::Cancel, this);
    layout->addWidget(buttonBox_);

    setMinimumWidth(400);

    // Connections
    connect(currentPasswordEdit_, &QLineEdit::textChanged,
            this, &MasterPasswordDialog::updateOkButtonState);
    connect(newPasswordEdit_, &QLineEdit::textChanged,
            this, &MasterPasswordDialog::updatePasswordMatchIndicator);
    connect(newPasswordEdit_, &QLineEdit::textChanged,
            this, &MasterPasswordDialog::updateOkButtonState);
    connect(confirmPasswordEdit_, &QLineEdit::textChanged,
            this, &MasterPasswordDialog::updatePasswordMatchIndicator);
    connect(confirmPasswordEdit_, &QLineEdit::textChanged,
            this, &MasterPasswordDialog::updateOkButtonState);
    connect(buttonBox_, &QDialogButtonBox::accepted,
            this, &MasterPasswordDialog::onOkClicked);
    connect(buttonBox_, &QDialogButtonBox::rejected,
            this, &QDialog::reject);

    updateOkButtonState();

    BOOST_LOG_SEV(lg(), debug) << "Master password dialog initialized in mode: "
                               << static_cast<int>(mode_);
}

QString MasterPasswordDialog::getPassword() const {
    return currentPasswordEdit_->text();
}

QString MasterPasswordDialog::getNewPassword() const {
    return newPasswordEdit_->text();
}

void MasterPasswordDialog::onOkClicked() {
    if (validateInput()) {
        accept();
    }
}

void MasterPasswordDialog::updatePasswordMatchIndicator() {
    if (mode_ == Mode::Unlock) {
        matchIndicatorLabel_->clear();
        return;
    }

    QString newPass = newPasswordEdit_->text();
    QString confirmPass = confirmPasswordEdit_->text();

    if (confirmPass.isEmpty()) {
        matchIndicatorLabel_->clear();
    } else if (newPass == confirmPass) {
        matchIndicatorLabel_->setStyleSheet("color: green;");
        matchIndicatorLabel_->setText(tr("Passwords match"));
    } else {
        matchIndicatorLabel_->setStyleSheet("color: red;");
        matchIndicatorLabel_->setText(tr("Passwords do not match"));
    }
}

void MasterPasswordDialog::updateOkButtonState() {
    auto* okButton = buttonBox_->button(QDialogButtonBox::Ok);
    bool valid = false;

    switch (mode_) {
    case Mode::Unlock:
        valid = !currentPasswordEdit_->text().isEmpty();
        break;

    case Mode::Create:
        // Allow blank password - just require both fields match
        valid = newPasswordEdit_->text() == confirmPasswordEdit_->text();
        break;

    case Mode::Change:
        valid = !currentPasswordEdit_->text().isEmpty() &&
                !newPasswordEdit_->text().isEmpty() &&
                newPasswordEdit_->text() == confirmPasswordEdit_->text();
        break;
    }

    okButton->setEnabled(valid);
}

bool MasterPasswordDialog::validateInput() {
    switch (mode_) {
    case Mode::Unlock:
        if (currentPasswordEdit_->text().isEmpty()) {
            QMessageBox::warning(this, tr("Validation Error"),
                tr("Please enter your master password."));
            currentPasswordEdit_->setFocus();
            return false;
        }
        break;

    case Mode::Create:
        // Allow blank password - just require both fields match
        if (newPasswordEdit_->text() != confirmPasswordEdit_->text()) {
            QMessageBox::warning(this, tr("Validation Error"),
                tr("Passwords do not match."));
            confirmPasswordEdit_->setFocus();
            return false;
        }
        break;

    case Mode::Change:
        if (currentPasswordEdit_->text().isEmpty()) {
            QMessageBox::warning(this, tr("Validation Error"),
                tr("Please enter your current password."));
            currentPasswordEdit_->setFocus();
            return false;
        }
        if (newPasswordEdit_->text().isEmpty()) {
            QMessageBox::warning(this, tr("Validation Error"),
                tr("Please enter a new password."));
            newPasswordEdit_->setFocus();
            return false;
        }
        if (newPasswordEdit_->text() != confirmPasswordEdit_->text()) {
            QMessageBox::warning(this, tr("Validation Error"),
                tr("New passwords do not match."));
            confirmPasswordEdit_->setFocus();
            return false;
        }
        break;
    }

    return true;
}

}
