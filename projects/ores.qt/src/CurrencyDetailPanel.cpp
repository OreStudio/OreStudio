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
#include "ores.qt/CurrencyDetailPanel.hpp"

#include <QtConcurrent>
#include <QFutureWatcher>
#include <QVBoxLayout>
#include <QToolBar>
#include <QIcon>
#include <QPixmap>
#include <QImage>
#include <QPainter>
#include "ui_CurrencyDetailPanel.h"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.risk/messaging/protocol.hpp"
#include "ores.comms/protocol/frame.hpp"

namespace ores::qt {

using comms::protocol::frame;
using comms::protocol::message_type;
using namespace ores::utility::log;
using FutureResult = std::pair<bool, std::string>;

QIcon CurrencyDetailPanel::createRecoloredIcon(const QString& svgPath, const QColor& color) {
    QIcon originalIcon(svgPath);
    if (originalIcon.isNull()) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to load icon: " << svgPath.toStdString();
        return {};
    }

    QIcon coloredIcon;
    const QColor disabledColor(50, 50, 50); // Dark gray for disabled state

    for (int size : {16, 20, 24, 32, 48, 64}) {
        QPixmap pixmap = originalIcon.pixmap(size, size);

        // Create normal state image
        QImage normalImage = pixmap.toImage().convertToFormat(QImage::Format_ARGB32);
        for (int y = 0; y < normalImage.height(); ++y) {
            for (int x = 0; x < normalImage.width(); ++x) {
                QColor pixelColor = normalImage.pixelColor(x, y);
                if (pixelColor.alpha() > 0) {
                    pixelColor.setRed(color.red());
                    pixelColor.setGreen(color.green());
                    pixelColor.setBlue(color.blue());
                    normalImage.setPixelColor(x, y, pixelColor);
                }
            }
        }
        coloredIcon.addPixmap(QPixmap::fromImage(normalImage), QIcon::Normal);

        // Create disabled state image
        QImage disabledImage = pixmap.toImage().convertToFormat(QImage::Format_ARGB32);
        for (int y = 0; y < disabledImage.height(); ++y) {
            for (int x = 0; x < disabledImage.width(); ++x) {
                QColor pixelColor = disabledImage.pixelColor(x, y);
                if (pixelColor.alpha() > 0) {
                    pixelColor.setRed(disabledColor.red());
                    pixelColor.setGreen(disabledColor.green());
                    pixelColor.setBlue(disabledColor.blue());
                    disabledImage.setPixelColor(x, y, pixelColor);
                }
            }
        }
        coloredIcon.addPixmap(QPixmap::fromImage(disabledImage), QIcon::Disabled);
    }

    return coloredIcon;
}

CurrencyDetailPanel::CurrencyDetailPanel(QWidget* parent)
    : QWidget(parent), ui_(new Ui::CurrencyDetailPanel), isDirty_(false),
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
    saveAction_->setIcon(createRecoloredIcon(":/icons/ic_fluent_save_20_filled.svg",
            iconColor));
    saveAction_->setToolTip("Save changes");
    connect(saveAction_, &QAction::triggered, this,
        &CurrencyDetailPanel::onSaveClicked);
    toolBar_->addAction(saveAction_);

    // Create Delete action
    deleteAction_ = new QAction("Delete", this);
    deleteAction_->setIcon(createRecoloredIcon(
            ":/icons/ic_fluent_delete_20_regular.svg", iconColor));
    deleteAction_->setToolTip("Delete currency");
    connect(deleteAction_, &QAction::triggered, this,
        &CurrencyDetailPanel::onDeleteClicked);
    toolBar_->addAction(deleteAction_);

    // Add toolbar to the panel's layout
    // Get the main layout from the UI
    auto* mainLayout = qobject_cast<QVBoxLayout*>(layout());
    if (mainLayout)
        mainLayout->insertWidget(0, toolBar_);

    // Connect signals for editable fields to detect changes
    connect(ui_->isoCodeEdit, &QLineEdit::textChanged, this,
        &CurrencyDetailPanel::onFieldChanged);
    connect(ui_->nameEdit, &QLineEdit::textChanged, this,
        &CurrencyDetailPanel::onFieldChanged);
    connect(ui_->numericCodeEdit, &QLineEdit::textChanged, this,
        &CurrencyDetailPanel::onFieldChanged);
    connect(ui_->symbolEdit, &QLineEdit::textChanged, this,
        &CurrencyDetailPanel::onFieldChanged);
    connect(ui_->fractionSymbolEdit, &QLineEdit::textChanged, this,
        &CurrencyDetailPanel::onFieldChanged);
    connect(ui_->fractionsPerUnitSpinBox,
        QOverload<int>::of(&QSpinBox::valueChanged), this,
        &CurrencyDetailPanel::onFieldChanged);
    connect(ui_->roundingTypeEdit, &QLineEdit::textChanged, this,
        &CurrencyDetailPanel::onFieldChanged);
    connect(ui_->roundingPrecisionSpinBox, QOverload<int>::of(
            &QSpinBox::valueChanged), this,
        &CurrencyDetailPanel::onFieldChanged);
    connect(ui_->formatEdit, &QLineEdit::textChanged, this,
        &CurrencyDetailPanel::onFieldChanged);
    connect(ui_->currencyTypeEdit, &QLineEdit::textChanged, this,
        &CurrencyDetailPanel::onFieldChanged);

    // Initially disable save/reset buttons
    updateSaveResetButtonState();
}

void CurrencyDetailPanel::setClient(std::shared_ptr<comms::client> client) {
    client_ = std::move(client);
}

void CurrencyDetailPanel::setUsername(const std::string& username) {
    username_ = username;
}

CurrencyDetailPanel::~CurrencyDetailPanel() {
    const auto watchers = findChildren<QFutureWatcherBase*>();
    for (auto* watcher : watchers) {
        disconnect(watcher, nullptr, this, nullptr);
        watcher->cancel();
        watcher->waitForFinished();
    }
}

void CurrencyDetailPanel::setCurrency(const risk::domain::currency& currency) {
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

risk::domain::currency CurrencyDetailPanel::getCurrency() const {
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

    BOOST_LOG_SEV(lg(), debug) << "Save clicked for currency: "
                               << currentCurrency_.iso_code;

    risk::domain::currency currency = getCurrency();

    QPointer<CurrencyDetailPanel> self = this;
    QFuture<FutureResult> future =
        QtConcurrent::run([self, currency]() -> FutureResult {
            if (!self) return {false, ""};
                BOOST_LOG_SEV(lg(), debug) << "Sending save currency request for: "
                                           << currency.iso_code;

                if (!self->client_ || !self->client_->is_connected())
                    return {false, "Client disconnected during save request."};

                frame request_frame;
                if (self->isAddMode_) {
                    risk::messaging::create_currency_request request{currency};
                    auto payload = request.serialize();
                    request_frame = frame(message_type::create_currency_request,
                        0, std::move(payload));
                } else {
                    risk::messaging::update_currency_request request{currency};
                    auto payload = request.serialize();
                    request_frame = frame(message_type::update_currency_request,
                        0, std::move(payload));
                }

                auto response_result =
                    self->client_->send_request_sync(std::move(request_frame));

                if (!response_result)
                    return {false, "Failed to communicate with server"};

                BOOST_LOG_SEV(lg(), debug) << "Received save currency response.";
                bool result = false;
                std::string message = "Invalid server response";
                if (self->isAddMode_) {
                    using risk::messaging::create_currency_response;
                    auto response = create_currency_response::
                        deserialize(response_result->payload());
                    if (response) {
                        result = true;
                        message = response->message;
                    }
                } else {
                    using risk::messaging::update_currency_response;
                    auto response = update_currency_response::
                        deserialize(response_result->payload());
                    if (response) {
                        result = true;
                        message = response->message;
                    }
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

void CurrencyDetailPanel::onResetClicked() {
    BOOST_LOG_SEV(lg(), debug) << "Reset clicked for currency: "
                               << currentCurrency_.iso_code;
    setCurrency(currentCurrency_);
}

void CurrencyDetailPanel::onDeleteClicked() {
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

    QPointer<CurrencyDetailPanel> self = this;
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
            self->clearPanel();
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

void CurrencyDetailPanel::onFieldChanged() {
    isDirty_ = true;
    emit isDirtyChanged(true);
    updateSaveResetButtonState();
}

void CurrencyDetailPanel::updateSaveResetButtonState() {
    if (saveAction_)
        saveAction_->setEnabled(isDirty_);

    if (deleteAction_)
        deleteAction_->setEnabled(!isAddMode_);
}

}
