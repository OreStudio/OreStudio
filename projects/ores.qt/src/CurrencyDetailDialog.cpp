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
#include "ores.qt/CurrencyDetailDialog.hpp"

#include <QtConcurrent>
#include <QFutureWatcher>
#include <QVBoxLayout>
#include <QToolBar>
#include <QIcon>
#include <QPixmap>
#include <QImage>
#include <QPainter>
#include "ui_CurrencyDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.risk/messaging/protocol.hpp"
#include "ores.comms/protocol/frame.hpp"

namespace ores::qt {

using comms::protocol::frame;
using comms::protocol::message_type;
using namespace ores::utility::log;
using FutureResult = std::pair<bool, std::string>;

CurrencyDetailDialog::CurrencyDetailDialog(QWidget* parent)
    : QWidget(parent), ui_(new Ui::CurrencyDetailDialog), isDirty_(false),
      isAddMode_(false) {

    ui_->setupUi(this);

    // Create toolbar
    toolBar_ = new QToolBar(this);
    toolBar_->setMovable(false);
    toolBar_->setFloatable(false);

    // Define icon color (light gray for dark theme)
    const QColor iconColor(220, 220, 220);

    // Create Save action
    saveAction_ = new QAction("Save", this);
    saveAction_->setIcon(IconUtils::createRecoloredIcon(":/icons/ic_fluent_save_20_filled.svg",
            iconColor));
    saveAction_->setToolTip("Save changes");
    connect(saveAction_, &QAction::triggered, this,
        &CurrencyDetailDialog::onSaveClicked);
    toolBar_->addAction(saveAction_);

    // Create Delete action
    deleteAction_ = new QAction("Delete", this);
    deleteAction_->setIcon(IconUtils::createRecoloredIcon(
            ":/icons/ic_fluent_delete_20_regular.svg", iconColor));
    deleteAction_->setToolTip("Delete currency");
    connect(deleteAction_, &QAction::triggered, this,
        &CurrencyDetailDialog::onDeleteClicked);
    toolBar_->addAction(deleteAction_);

    // Add toolbar to the dialog's layout
    // Get the main layout from the UI
    auto* mainLayout = qobject_cast<QVBoxLayout*>(layout());
    if (mainLayout)
        mainLayout->insertWidget(0, toolBar_);

    // Connect signals for editable fields to detect changes
    connect(ui_->isoCodeEdit, &QLineEdit::textChanged, this,
        &CurrencyDetailDialog::onFieldChanged);
    connect(ui_->nameEdit, &QLineEdit::textChanged, this,
        &CurrencyDetailDialog::onFieldChanged);
    connect(ui_->numericCodeEdit, &QLineEdit::textChanged, this,
        &CurrencyDetailDialog::onFieldChanged);
    connect(ui_->symbolEdit, &QLineEdit::textChanged, this,
        &CurrencyDetailDialog::onFieldChanged);
    connect(ui_->fractionSymbolEdit, &QLineEdit::textChanged, this,
        &CurrencyDetailDialog::onFieldChanged);
    connect(ui_->fractionsPerUnitSpinBox,
        QOverload<int>::of(&QSpinBox::valueChanged), this,
        &CurrencyDetailDialog::onFieldChanged);
    connect(ui_->roundingTypeEdit, &QLineEdit::textChanged, this,
        &CurrencyDetailDialog::onFieldChanged);
    connect(ui_->roundingPrecisionSpinBox, QOverload<int>::of(
            &QSpinBox::valueChanged), this,
        &CurrencyDetailDialog::onFieldChanged);
    connect(ui_->formatEdit, &QLineEdit::textChanged, this,
        &CurrencyDetailDialog::onFieldChanged);
    connect(ui_->currencyTypeEdit, &QLineEdit::textChanged, this,
        &CurrencyDetailDialog::onFieldChanged);

    // Initially disable save/reset buttons
    updateSaveResetButtonState();
}

void CurrencyDetailDialog::setClient(std::shared_ptr<comms::net::client> client) {
    client_ = std::move(client);
}

void CurrencyDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

CurrencyDetailDialog::~CurrencyDetailDialog() {
    const auto watchers = findChildren<QFutureWatcherBase*>();
    for (auto* watcher : watchers) {
        disconnect(watcher, nullptr, this, nullptr);
        watcher->cancel();
        watcher->waitForFinished();
    }
}

void CurrencyDetailDialog::setCurrency(const risk::domain::currency& currency) {
    currentCurrency_ = currency;
    isAddMode_ = currency.iso_code.empty();

    ui_->isoCodeEdit->setReadOnly(!isAddMode_);
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

risk::domain::currency CurrencyDetailDialog::getCurrency() const {
    risk::domain::currency currency = currentCurrency_;
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
    currency.modified_by = username_.empty() ? "qt_user" : username_;

    return currency;
}

void CurrencyDetailDialog::clearDialog() {
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

void CurrencyDetailDialog::save() {
    onSaveClicked();
}

void CurrencyDetailDialog::onSaveClicked() {
    if (!client_ || !client_->is_connected()) {
        BOOST_LOG_SEV(lg(), warn) << "Save clicked but client not connected.";
        emit errorMessage("Not connected to server. Please login.");
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Save clicked for currency: "
                               << currentCurrency_.iso_code;

    risk::domain::currency currency = getCurrency();

    QPointer<CurrencyDetailDialog> self = this;
    QFuture<FutureResult> future =
        QtConcurrent::run([self, currency]() -> FutureResult {
            if (!self) return {false, ""};
                BOOST_LOG_SEV(lg(), debug) << "Sending save currency request for: "
                                           << currency.iso_code;

                if (!self->client_ || !self->client_->is_connected())
                    return {false, "Client disconnected during save request."};

                // Use single save_currency message for both create and update
                risk::messaging::save_currency_request request{currency};
                auto payload = request.serialize();
                frame request_frame = frame(message_type::save_currency_request,
                    0, std::move(payload));

                auto response_result =
                    self->client_->send_request_sync(std::move(request_frame));

                if (!response_result)
                    return {false, "Failed to communicate with server"};

                BOOST_LOG_SEV(lg(), debug) << "Received save currency response.";

                using risk::messaging::save_currency_response;
                auto response = save_currency_response::
                    deserialize(response_result->payload());

                bool result = false;
                std::string message = "Invalid server response";
                if (response) {
                    result = response->success;
                    message = response->message;
                }

                return {result, message};
            });

    auto* watcher = new QFutureWatcher<FutureResult>(self);
    connect(watcher, &QFutureWatcher<FutureResult>::finished, self,
        [self, watcher, currency]() {

        if (!self)
            return;

        auto [success, message] = watcher->result();
        watcher->deleteLater();

        if (success) {
            BOOST_LOG_SEV(lg(), debug) << "Currency saved successfully.";

            emit self->statusMessage(QString("Successfully saved currency: %1")
                .arg(QString::fromStdString(currency.iso_code)));

            self->isDirty_ = false;
            emit self->isDirtyChanged(false);
            self->updateSaveResetButtonState();

            // Transition from add mode to edit mode after successful creation
            if (self->isAddMode_) {
                self->isAddMode_ = false;
                self->currentCurrency_ = currency; // Update with saved currency
                self->ui_->isoCodeEdit->setReadOnly(true); // ISO code can't be changed anymore
                emit self->currencyCreated(
                    QString::fromStdString(currency.iso_code));
            } else {
                self->currentCurrency_ = currency; // Update with modified currency
                emit self->currencyUpdated(
                    QString::fromStdString(currency.iso_code));
            }
        } else {
            BOOST_LOG_SEV(lg(), error) << "Currency save failed: " << message;
            emit self->errorMessage(QString("Failed to save currency: %1")
                .arg(QString::fromStdString(message)));
            MessageBoxHelper::critical(self, "Save Failed",
                QString::fromStdString(message));
        }
    });

    watcher->setFuture(future);
}

void CurrencyDetailDialog::onResetClicked() {
    BOOST_LOG_SEV(lg(), debug) << "Reset clicked for currency: "
                               << currentCurrency_.iso_code;
    setCurrency(currentCurrency_);
}

void CurrencyDetailDialog::onDeleteClicked() {
    if (!client_ || !client_->is_connected()) {
        BOOST_LOG_SEV(lg(), warn) << "Delete clicked but client not connected.";
        emit errorMessage("Not connected to server. Please login.");
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Delete request for currency: "
                               << currentCurrency_.iso_code;

    auto reply =
        MessageBoxHelper::question(this, "Delete Currency",
        QString("Are you sure you want to delete currency '%1' (%2)?")
        .arg(QString::fromStdString(currentCurrency_.name))
        .arg(QString::fromStdString(currentCurrency_.iso_code)),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        BOOST_LOG_SEV(lg(), debug) << "Delete cancelled by user";
        return;
    }

    QPointer<CurrencyDetailDialog> self = this;
    const std::string iso_code = currentCurrency_.iso_code;
    QFuture<FutureResult> future =
        QtConcurrent::run([self, iso_code]() -> FutureResult {
            if (!self) return {false, {}};
            BOOST_LOG_SEV(lg(), debug) << "Sending delete currency request for: "
                                       << iso_code;

            if (!self->client_ || !self->client_->is_connected()) {
                BOOST_LOG_SEV(lg(), error) << "Client disconnected during operation.";
                return {false, "Client disconnected during operation."};
            }

            risk::messaging::delete_currency_request request{iso_code};
            auto payload = request.serialize();

            frame request_frame(message_type::delete_currency_request,
                    0, std::move(payload));

            auto response_result =
                self->client_->send_request_sync(std::move(request_frame));

            if (!response_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to communicate with server.";
                return {false, "Failed to communicate with server."};
            }

            BOOST_LOG_SEV(lg(), debug) << "Received delete currency response.";
            auto response = risk::messaging::delete_currency_response::deserialize(
                response_result->payload());

            if (!response) {
                BOOST_LOG_SEV(lg(), error) << "Invalid server response";
                return {false, "Invalid server response"};
            }

            return {response->success, response->message};
        });


    auto* watcher = new QFutureWatcher<FutureResult>(self);
    connect(watcher, &QFutureWatcher<FutureResult>::finished, self,
        [self, watcher, iso_code]() {

        BOOST_LOG_SEV(lg(), debug) << "Received currencies delete result.";
        if (!self) return;
        auto [success, message] = watcher->result();
        watcher->deleteLater();

        if (success) {
            BOOST_LOG_SEV(lg(), debug) << "Currency deleted successfully.";
            emit self->statusMessage(QString("Successfully deleted currency: %1")
                .arg(QString::fromStdString(iso_code)));
            emit self->currencyDeleted(QString::fromStdString(iso_code));
            self->clearDialog();
        } else {
            BOOST_LOG_SEV(lg(), error) << "Currency deletion failed: "
                                       << message;
            emit self->errorMessage(QString("Failed to delete currency: %1")
                .arg(QString::fromStdString(message)));
            MessageBoxHelper::critical(self, "Delete Failed",
                QString::fromStdString(message));
        }
    });

    watcher->setFuture(future);
}

void CurrencyDetailDialog::onFieldChanged() {
    isDirty_ = true;
    emit isDirtyChanged(true);
    updateSaveResetButtonState();
}

void CurrencyDetailDialog::updateSaveResetButtonState() {
    if (saveAction_)
        saveAction_->setEnabled(isDirty_);

    if (deleteAction_)
        deleteAction_->setEnabled(!isAddMode_);
}

}
