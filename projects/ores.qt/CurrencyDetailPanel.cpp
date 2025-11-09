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
#include "ui_CurrencyDetailPanel.h"
#include "ores.qt/CurrencyDetailPanel.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.risk/messaging/protocol.hpp"
#include "ores.comms/protocol/frame.hpp"

namespace ores::qt {

using namespace ores::utility::log;

namespace {
    auto& lg() {
        static auto instance = make_logger("ores.qt.currency_detail_panel");
        return instance;
    }
}

CurrencyDetailPanel::CurrencyDetailPanel(QWidget* parent)
    : QWidget(parent), ui_(new Ui::CurrencyDetailPanel), isDirty_(false), is_add_mode_(false) { // Removed client from constructor, initialized client_ to nullptr implicitly
    ui_->setupUi(this);

    // Connect signals for editable fields to detect changes
    connect(ui_->isoCodeEdit, &QLineEdit::textChanged, this, &CurrencyDetailPanel::onFieldChanged);
    connect(ui_->nameEdit, &QLineEdit::textChanged, this, &CurrencyDetailPanel::onFieldChanged);
    connect(ui_->numericCodeEdit, &QLineEdit::textChanged, this, &CurrencyDetailPanel::onFieldChanged);
    connect(ui_->symbolEdit, &QLineEdit::textChanged, this, &CurrencyDetailPanel::onFieldChanged);
    connect(ui_->fractionSymbolEdit, &QLineEdit::textChanged, this, &CurrencyDetailPanel::onFieldChanged);
    connect(ui_->fractionsPerUnitSpinBox, QOverload<int>::of(&QSpinBox::valueChanged), this, &CurrencyDetailPanel::onFieldChanged);
    connect(ui_->roundingTypeEdit, &QLineEdit::textChanged, this, &CurrencyDetailPanel::onFieldChanged);
    connect(ui_->roundingPrecisionSpinBox, QOverload<int>::of(&QSpinBox::valueChanged), this, &CurrencyDetailPanel::onFieldChanged);
    connect(ui_->formatEdit, &QLineEdit::textChanged, this, &CurrencyDetailPanel::onFieldChanged);
    connect(ui_->currencyTypeEdit, &QLineEdit::textChanged, this, &CurrencyDetailPanel::onFieldChanged);

    // Initially disable save/reset buttons
    updateSaveResetButtonState();
}

void CurrencyDetailPanel::setClient(std::shared_ptr<comms::client> client) {
    client_ = std::move(client);
}

void CurrencyDetailPanel::setUsername(const std::string& username) {
    username_ = username;
}

CurrencyDetailPanel::~CurrencyDetailPanel() = default;

void CurrencyDetailPanel::setCurrency(const risk::domain::currency& currency) {
    currentCurrency_ = currency;
    is_add_mode_ = currency.iso_code.empty();

    ui_->isoCodeEdit->setReadOnly(!is_add_mode_);
    ui_->isoCodeEdit->setText(QString::fromStdString(currency.iso_code));
    ui_->nameEdit->setText(QString::fromStdString(currency.name));
    ui_->numericCodeEdit->setText(QString::fromStdString(currency.numeric_code));
    ui_->symbolEdit->setText(QString::fromStdString(currency.symbol));
    ui_->fractionSymbolEdit->setText(QString::fromStdString(currency.fraction_symbol));
    ui_->fractionsPerUnitSpinBox->setValue(currency.fractions_per_unit);
    ui_->roundingTypeEdit->setText(QString::fromStdString(currency.rounding_type));
    ui_->roundingPrecisionSpinBox->setValue(currency.rounding_precision);
    ui_->formatEdit->setText(QString::fromStdString(currency.format));
    ui_->currencyTypeEdit->setText(QString::fromStdString(currency.currency_type));
    ui_->modifiedByEdit->setText(QString::fromStdString(currency.modified_by));
    ui_->validFromEdit->setText(QString::fromStdString(currency.valid_from));
    ui_->validToEdit->setText(QString::fromStdString(currency.valid_to));

    isDirty_ = false;
    emit isDirtyChanged(false);
    updateSaveResetButtonState();
}

risk::domain::currency CurrencyDetailPanel::getCurrency() const {
    risk::domain::currency currency = currentCurrency_; // Start with original to keep read-only fields
    currency.iso_code = ui_->isoCodeEdit->text().toStdString();
    currency.name = ui_->nameEdit->text().toStdString();
    currency.numeric_code = ui_->numericCodeEdit->text().toStdString();
    currency.symbol = ui_->symbolEdit->text().toStdString();
    currency.fraction_symbol = ui_->fractionSymbolEdit->text().toStdString();
    currency.fractions_per_unit = ui_->fractionsPerUnitSpinBox->value();
    currency.rounding_type = ui_->roundingTypeEdit->text().toStdString();
    currency.rounding_precision = ui_->roundingPrecisionSpinBox->value();
    currency.format = ui_->formatEdit->text().toStdString();
    currency.currency_type = ui_->currencyTypeEdit->text().toStdString();

    // Set modified_by to application user (trigger will set valid_from/valid_to)
    currency.modified_by = username_.empty() ? "qt_user" : username_;

    return currency;
}

void CurrencyDetailPanel::clearPanel() {
    ui_->isoCodeEdit->clear();
    ui_->nameEdit->clear();
    ui_->numericCodeEdit->clear();
    ui_->symbolEdit->clear();
    ui_->fractionSymbolEdit->clear();
    ui_->fractionsPerUnitSpinBox->clear();
    ui_->roundingTypeEdit->clear();
    ui_->roundingPrecisionSpinBox->clear();
    ui_->formatEdit->clear();
    ui_->currencyTypeEdit->clear();
    ui_->modifiedByEdit->clear();
    ui_->validFromEdit->clear();
    ui_->validToEdit->clear();

    isDirty_ = false;
    emit isDirtyChanged(false);
    updateSaveResetButtonState();
}

void CurrencyDetailPanel::save() {
    onSaveClicked();
}

void CurrencyDetailPanel::onSaveClicked() {
    if (!client_ || !client_->is_connected()) {
        BOOST_LOG_SEV(lg(), warn) << "Save clicked but client not connected.";
        emit errorMessage("Not connected to server. Please login.");
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Save clicked for currency: " << currentCurrency_.iso_code;

    risk::domain::currency currency = getCurrency();

    // Send update request asynchronously
    QFuture<std::pair<bool, std::string>> future =
        QtConcurrent::run([this, currency]() -> std::pair<bool, std::string> {
            BOOST_LOG_SEV(lg(), info) << "Sending save request for: "
                                      << currency.iso_code;

            // Ensure client is still valid in the background thread
            if (!client_ || !client_->is_connected()) {
                return {false, "Client disconnected during operation."};
            }

            comms::protocol::frame request_frame;
            if (is_add_mode_) {
                risk::messaging::create_currency_request request{currency};
                auto payload = request.serialize();
                request_frame = comms::protocol::frame(
                    comms::protocol::message_type::create_currency_request,
                    0,
                    std::move(payload)
                );
            } else {
                risk::messaging::update_currency_request request{currency};
                auto payload = request.serialize();
                request_frame = comms::protocol::frame(
                    comms::protocol::message_type::update_currency_request,
                    0,
                    std::move(payload)
                );
            }


            // Send request synchronously (on background thread)
            auto response_result = client_->send_request_sync(std::move(request_frame));

            if (!response_result) {
                return {false, "Failed to communicate with server"};
            }

            BOOST_LOG_SEV(lg(), info) << "Received save response.";
            bool result = false;
            std::string message = "Invalid server response";
            if (is_add_mode_) {
                using risk::messaging::create_currency_response;
                auto response = create_currency_response::deserialize(response_result->payload());
                if (response) {
                    result = true;
                    message = response->message;
                }
            } else {
                using risk::messaging::update_currency_response;
                auto response = update_currency_response::deserialize(response_result->payload());
                if (response) {
                    result = true;
                    message = response->message;
                }
            }

            return {result, message};
        });

    // Use a watcher to handle the result
    auto* watcher = new QFutureWatcher<std::pair<bool, std::string>>(this);
    connect(watcher, &QFutureWatcher<std::pair<bool, std::string>>::finished,
            this, [this, watcher, currency]() {
        auto [success, message] = watcher->result();
        watcher->deleteLater();

        if (success) {
            BOOST_LOG_SEV(lg(), info) << "Currency saved successfully";

            // Show success message
            MessageBoxHelper::information(this, "Save Successful",
                QString("Currency '%1' saved successfully.")
                .arg(QString::fromStdString(currency.iso_code)));

            emit statusMessage(QString("Successfully saved currency: %1")
                .arg(QString::fromStdString(currency.iso_code)));

            isDirty_ = false;
            emit isDirtyChanged(false);
            updateSaveResetButtonState();

            // Transition from add mode to edit mode after successful creation
            if (is_add_mode_) {
                is_add_mode_ = false;
                currentCurrency_ = currency; // Update with saved currency
                ui_->isoCodeEdit->setReadOnly(true); // ISO code can't be changed anymore
                emit currencyCreated();
            } else {
                currentCurrency_ = currency; // Update with modified currency
                emit currencyUpdated();
            }
        } else {
            BOOST_LOG_SEV(lg(), error) << "Currency save failed: " << message;
            emit errorMessage(QString("Failed to save currency: %1")
                .arg(QString::fromStdString(message)));
            MessageBoxHelper::critical(this, "Save Failed",
                QString::fromStdString(message));
        }
    });

    watcher->setFuture(future);
}

void CurrencyDetailPanel::onResetClicked() {
    BOOST_LOG_SEV(lg(), info) << "Reset clicked for currency: " << currentCurrency_.iso_code;
    setCurrency(currentCurrency_); // Revert to original values
}

void CurrencyDetailPanel::onDeleteClicked() {
    if (!client_ || !client_->is_connected()) {
        BOOST_LOG_SEV(lg(), warn) << "Delete clicked but client not connected.";
        emit errorMessage("Not connected to server. Please login.");
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Delete request for currency: " << currentCurrency_.iso_code;

    // Confirm deletion
    auto reply = MessageBoxHelper::question(this, "Delete Currency",
        QString("Are you sure you want to delete currency '%1' (%2)?")
            .arg(QString::fromStdString(currentCurrency_.name))
            .arg(QString::fromStdString(currentCurrency_.iso_code)),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        BOOST_LOG_SEV(lg(), info) << "Delete cancelled by user";
        return;
    }

    // Store currency ISO code for the async operation
    const std::string iso_code = currentCurrency_.iso_code;

    // Send delete request asynchronously
    QFuture<std::pair<bool, std::string>> future =
        QtConcurrent::run([this, iso_code]() -> std::pair<bool, std::string> {
            BOOST_LOG_SEV(lg(), info) << "Sending delete_currency_request for: "
                                      << iso_code;

            // Ensure client is still valid in the background thread
            if (!client_ || !client_->is_connected()) {
                return {false, "Client disconnected during operation."};
            }

            risk::messaging::delete_currency_request request{iso_code};
            auto payload = request.serialize();

            comms::protocol::frame request_frame(
                comms::protocol::message_type::delete_currency_request,
                0,
                std::move(payload)
            );

            // Send request synchronously (on background thread)
            auto response_result = client_->send_request_sync(std::move(request_frame));

            if (!response_result) {
                return {false, "Failed to communicate with server"};
            }

            BOOST_LOG_SEV(lg(), info) << "Received delete_currency_response";
            auto response = risk::messaging::delete_currency_response::deserialize(
                response_result->payload()
            );

            if (!response) {
                return {false, "Invalid server response"};
            }

            return {response->success, response->message};
        });

    // Use a watcher to handle the result
    auto* watcher = new QFutureWatcher<std::pair<bool, std::string>>(this);
    connect(watcher, &QFutureWatcher<std::pair<bool, std::string>>::finished,
            this, [this, watcher, iso_code]() {
        auto [success, message] = watcher->result();
        watcher->deleteLater();

        if (success) {
            BOOST_LOG_SEV(lg(), info) << "Currency deleted successfully";
            emit statusMessage(QString("Successfully deleted currency: %1")
                .arg(QString::fromStdString(iso_code)));
            emit currencyDeleted(QString::fromStdString(iso_code)); // Notify parent
            clearPanel(); // Clear panel after deletion
        } else {
            BOOST_LOG_SEV(lg(), error) << "Currency deletion failed: " << message;
            emit errorMessage(QString("Failed to delete currency: %1")
                .arg(QString::fromStdString(message)));
            MessageBoxHelper::critical(this, "Delete Failed",
                QString::fromStdString(message));
        }
    });

    watcher->setFuture(future);
}

void CurrencyDetailPanel::onFieldChanged() {
    isDirty_ = true;
    emit isDirtyChanged(true);
    updateSaveResetButtonState();
}

void CurrencyDetailPanel::updateSaveResetButtonState() {
    // The save and reset buttons are now handled by the main window's actions
    // or by the panel itself if it were to have its own buttons.
    // For now, we just manage the 'dirty' state.
    // The main window will enable/disable its 'Save' and 'Reset' actions
    // based on this panel's dirty state.
}

} // namespace ores::qt
