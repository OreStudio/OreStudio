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
#include <QtConcurrent>
#include <QFutureWatcher>
#include "ui_CurrencyEditDialog.h"
#include "ores.qt/CurrencyEditDialog.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.risk/messaging/protocol.hpp"
#include "ores.comms/protocol/frame.hpp"

namespace ores::qt {

using namespace ores::utility::log;
using namespace ores::risk::domain;

CurrencyEditDialog::CurrencyEditDialog(const currency& curr,
                                       std::shared_ptr<comms::client> client,
                                       QWidget* parent)
    : QDialog(parent),
      ui_(new Ui::CurrencyEditDialog),
      original_(curr),
      client_(std::move(client)),
      has_changes_(false) {

    ui_->setupUi(this);

    // Set window title with currency ISO code
    setWindowTitle(QString("Edit Currency - %1").arg(QString::fromStdString(curr.iso_code)));

    // Populate fields with currency data
    populateFields();

    // Connect field change signals
    connect(ui_->nameEdit, &QLineEdit::textChanged,
            this, &CurrencyEditDialog::onFieldChanged);
    connect(ui_->numericCodeEdit, &QLineEdit::textChanged,
            this, &CurrencyEditDialog::onFieldChanged);
    connect(ui_->symbolEdit, &QLineEdit::textChanged,
            this, &CurrencyEditDialog::onFieldChanged);
    connect(ui_->fractionSymbolEdit, &QLineEdit::textChanged,
            this, &CurrencyEditDialog::onFieldChanged);
    connect(ui_->fractionsPerUnitSpinBox, QOverload<int>::of(&QSpinBox::valueChanged),
            this, &CurrencyEditDialog::onFieldChanged);
    connect(ui_->roundingTypeEdit, &QLineEdit::textChanged,
            this, &CurrencyEditDialog::onFieldChanged);
    connect(ui_->roundingPrecisionSpinBox, QOverload<int>::of(&QSpinBox::valueChanged),
            this, &CurrencyEditDialog::onFieldChanged);
    connect(ui_->formatEdit, &QLineEdit::textChanged,
            this, &CurrencyEditDialog::onFieldChanged);
    connect(ui_->currencyTypeEdit, &QLineEdit::textChanged,
            this, &CurrencyEditDialog::onFieldChanged);

    // Connect button signals
    connect(ui_->saveButton, &QPushButton::clicked,
            this, &CurrencyEditDialog::onSaveClicked);
    connect(ui_->resetButton, &QPushButton::clicked,
            this, &CurrencyEditDialog::onResetClicked);
    connect(ui_->deleteButton, &QPushButton::clicked,
            this, &CurrencyEditDialog::onDeleteClicked);
    connect(ui_->cancelButton, &QPushButton::clicked,
            this, &QDialog::close);

    BOOST_LOG_SEV(lg(), debug) << "Currency edit dialog created for: "
                              << curr.iso_code;
}

CurrencyEditDialog::~CurrencyEditDialog() {
    delete ui_;
}

void CurrencyEditDialog::populateFields() {
    ui_->isoCodeEdit->setText(QString::fromStdString(original_.iso_code));
    ui_->nameEdit->setText(QString::fromStdString(original_.name));
    ui_->numericCodeEdit->setText(QString::fromStdString(original_.numeric_code));
    ui_->symbolEdit->setText(QString::fromStdString(original_.symbol));
    ui_->fractionSymbolEdit->setText(QString::fromStdString(original_.fraction_symbol));
    ui_->fractionsPerUnitSpinBox->setValue(original_.fractions_per_unit);
    ui_->roundingTypeEdit->setText(QString::fromStdString(original_.rounding_type));
    ui_->roundingPrecisionSpinBox->setValue(original_.rounding_precision);
    ui_->formatEdit->setText(QString::fromStdString(original_.format));
    ui_->currencyTypeEdit->setText(QString::fromStdString(original_.currency_type));
    ui_->modifiedByEdit->setText(QString::fromStdString(original_.modified_by));
    ui_->validFromEdit->setText(QString::fromStdString(original_.valid_from));
    ui_->validToEdit->setText(QString::fromStdString(original_.valid_to));
}

void CurrencyEditDialog::resetFields() {
    populateFields();
    has_changes_ = false;
    updateSaveButtonState();
    BOOST_LOG_SEV(lg(), debug) << "Fields reset to original values";
}

bool CurrencyEditDialog::validateFields() {
    // Name is required
    if (ui_->nameEdit->text().trimmed().isEmpty()) {
        ui_->nameEdit->setStyleSheet("QLineEdit { border: 2px solid red; }");
        return false;
    } else {
        ui_->nameEdit->setStyleSheet("");
    }

    return true;
}

void CurrencyEditDialog::updateSaveButtonState() {
    const bool valid = validateFields();
    const bool changed = hasChanges();
    ui_->saveButton->setEnabled(valid && changed);

    BOOST_LOG_SEV(lg(), trace) << "Save button state: valid=" << valid
                              << ", changed=" << changed;
}

bool CurrencyEditDialog::hasChanges() const {
    if (ui_->nameEdit->text().toStdString() != original_.name) return true;
    if (ui_->numericCodeEdit->text().toStdString() != original_.numeric_code) return true;
    if (ui_->symbolEdit->text().toStdString() != original_.symbol) return true;
    if (ui_->fractionSymbolEdit->text().toStdString() != original_.fraction_symbol) return true;
    if (ui_->fractionsPerUnitSpinBox->value() != original_.fractions_per_unit) return true;
    if (ui_->roundingTypeEdit->text().toStdString() != original_.rounding_type) return true;
    if (ui_->roundingPrecisionSpinBox->value() != original_.rounding_precision) return true;
    if (ui_->formatEdit->text().toStdString() != original_.format) return true;
    if (ui_->currencyTypeEdit->text().toStdString() != original_.currency_type) return true;

    return false;
}

void CurrencyEditDialog::onFieldChanged() {
    has_changes_ = true;
    updateSaveButtonState();
}

void CurrencyEditDialog::onSaveClicked() {
    BOOST_LOG_SEV(lg(), info) << "Save button clicked for currency: "
                             << original_.iso_code;

    if (!validateFields()) {
        MessageBoxHelper::warning(this, "Validation Error",
            "Please correct the highlighted fields before saving.");
        return;
    }

    // Create updated currency object with values from the form
    currency updated = original_;
    updated.name = ui_->nameEdit->text().toStdString();
    updated.numeric_code = ui_->numericCodeEdit->text().toStdString();
    updated.symbol = ui_->symbolEdit->text().toStdString();
    updated.fraction_symbol = ui_->fractionSymbolEdit->text().toStdString();
    updated.fractions_per_unit = ui_->fractionsPerUnitSpinBox->value();
    updated.rounding_type = ui_->roundingTypeEdit->text().toStdString();
    updated.rounding_precision = ui_->roundingPrecisionSpinBox->value();
    updated.format = ui_->formatEdit->text().toStdString();
    updated.currency_type = ui_->currencyTypeEdit->text().toStdString();

    // Disable save button during request
    ui_->saveButton->setEnabled(false);

    // Send update request asynchronously
    QFuture<std::pair<bool, std::string>> future =
        QtConcurrent::run([this, updated]() -> std::pair<bool, std::string> {
            BOOST_LOG_SEV(lg(), info) << "Sending update_currency_request for: "
                                      << updated.iso_code;

            risk::messaging::update_currency_request request{updated};
            auto payload = request.serialize();

            comms::protocol::frame request_frame(
                comms::protocol::message_type::update_currency_request,
                0,
                std::move(payload)
            );

            // Send request synchronously (on background thread)
            auto response_result = client_->send_request_sync(std::move(request_frame));

            if (!response_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to send update request";
                return {false, "Failed to communicate with server"};
            }

            BOOST_LOG_SEV(lg(), info) << "Received update_currency_response";
            auto response = risk::messaging::update_currency_response::deserialize(
                response_result->payload()
            );

            if (!response) {
                BOOST_LOG_SEV(lg(), error) << "Failed to deserialize response";
                return {false, "Invalid server response"};
            }

            return {response->success, response->message};
        });

    // Use a watcher to handle the result
    auto* watcher = new QFutureWatcher<std::pair<bool, std::string>>(this);
    connect(watcher, &QFutureWatcher<std::pair<bool, std::string>>::finished,
            this, [this, watcher, updated]() {
        auto [success, message] = watcher->result();
        watcher->deleteLater();

        if (success) {
            BOOST_LOG_SEV(lg(), info) << "Currency updated successfully";

            // Emit status message
            emit statusMessage(QString("Successfully updated currency: %1")
                .arg(QString::fromStdString(updated.iso_code)));

            // Emit signal that currency was updated
            emit currencyUpdated();

            // Close dialog
            close();
        } else {
            BOOST_LOG_SEV(lg(), error) << "Currency update failed: " << message;

            // Emit error message
            emit errorMessage(QString("Failed to update currency: %1")
                .arg(QString::fromStdString(message)));

            MessageBoxHelper::critical(this, "Update Failed",
                QString::fromStdString(message));

            // Re-enable save button
            ui_->saveButton->setEnabled(true);
        }
    });

    watcher->setFuture(future);
}

void CurrencyEditDialog::onDeleteClicked() {
    BOOST_LOG_SEV(lg(), info) << "Delete button clicked for currency: "
                             << original_.iso_code;

    // Confirm deletion
    auto reply = MessageBoxHelper::question(this, "Delete Currency",
        QString("Are you sure you want to delete currency '%1' (%2)?")
            .arg(QString::fromStdString(original_.name))
            .arg(QString::fromStdString(original_.iso_code)),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    // Disable delete button during request
    ui_->deleteButton->setEnabled(false);

    // Send delete request asynchronously
    QFuture<std::pair<bool, std::string>> future =
        QtConcurrent::run([this]() -> std::pair<bool, std::string> {
            BOOST_LOG_SEV(lg(), info) << "Sending delete_currency_request for: "
                                      << original_.iso_code;

            risk::messaging::delete_currency_request request{original_.iso_code};
            auto payload = request.serialize();

            comms::protocol::frame request_frame(
                comms::protocol::message_type::delete_currency_request,
                0,
                std::move(payload)
            );

            // Send request synchronously (on background thread)
            auto response_result = client_->send_request_sync(std::move(request_frame));

            if (!response_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to send delete request";
                return {false, "Failed to communicate with server"};
            }

            BOOST_LOG_SEV(lg(), info) << "Received delete_currency_response";
            auto response = risk::messaging::delete_currency_response::deserialize(
                response_result->payload()
            );

            if (!response) {
                BOOST_LOG_SEV(lg(), error) << "Failed to deserialize response";
                return {false, "Invalid server response"};
            }

            return {response->success, response->message};
        });

    // Use a watcher to handle the result
    auto* watcher = new QFutureWatcher<std::pair<bool, std::string>>(this);
    connect(watcher, &QFutureWatcher<std::pair<bool, std::string>>::finished,
            this, [this, watcher]() {
        auto [success, message] = watcher->result();
        watcher->deleteLater();

        if (success) {
            BOOST_LOG_SEV(lg(), info) << "Currency deleted successfully";

            // Emit status message
            emit statusMessage(QString("Successfully deleted currency: %1")
                .arg(QString::fromStdString(original_.iso_code)));

            // Emit signal that currency was deleted
            emit currencyDeleted(QString::fromStdString(original_.iso_code));

            // Close dialog
            close();
        } else {
            BOOST_LOG_SEV(lg(), error) << "Currency deletion failed: " << message;

            // Emit error message
            emit errorMessage(QString("Failed to delete currency: %1")
                .arg(QString::fromStdString(message)));

            MessageBoxHelper::critical(this, "Delete Failed",
                QString::fromStdString(message));

            // Re-enable delete button
            ui_->deleteButton->setEnabled(true);
        }
    });

    watcher->setFuture(future);
}

void CurrencyEditDialog::onResetClicked() {
    BOOST_LOG_SEV(lg(), debug) << "Reset button clicked";
    resetFields();
}

}
