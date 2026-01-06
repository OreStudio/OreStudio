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
#include "ores.qt/FlagSelectorDialog.hpp"

#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QGroupBox>

namespace ores::qt {

using namespace ores::telemetry::log;

FlagSelectorDialog::FlagSelectorDialog(ImageCache* imageCache,
    const QString& currentImageId, QWidget* parent)
    : QDialog(parent),
      imageCache_(imageCache),
      currentImageId_(currentImageId) {

    BOOST_LOG_SEV(lg(), debug) << "Creating flag selector dialog, current: "
                               << currentImageId_.toStdString();

    setWindowTitle(tr("Select Flag"));
    setMinimumSize(500, 400);
    setupUi();

    // Connect to ImageCache signals
    connect(imageCache_, &ImageCache::imageListLoaded,
        this, &FlagSelectorDialog::onImageListLoaded);
    connect(imageCache_, &ImageCache::imageLoaded,
        this, &FlagSelectorDialog::onImageLoaded);

    // Load image list if not already loaded
    if (!imageCache_->hasImageList()) {
        imageCache_->loadImageList();
    } else {
        populateList();
    }
}

void FlagSelectorDialog::setupUi() {
    auto* mainLayout = new QVBoxLayout(this);

    // Search box
    auto* searchLayout = new QHBoxLayout();
    searchLayout->addWidget(new QLabel(tr("Search:")));
    searchEdit_ = new QLineEdit(this);
    searchEdit_->setPlaceholderText(tr("Filter by key or description..."));
    searchLayout->addWidget(searchEdit_);
    mainLayout->addLayout(searchLayout);

    // Content area with list and preview
    auto* contentLayout = new QHBoxLayout();

    // List of flags
    listWidget_ = new QListWidget(this);
    listWidget_->setIconSize(QSize(32, 32));
    listWidget_->setSelectionMode(QAbstractItemView::SingleSelection);
    contentLayout->addWidget(listWidget_, 3);

    // Preview panel
    auto* previewGroup = new QGroupBox(tr("Preview"), this);
    auto* previewLayout = new QVBoxLayout(previewGroup);

    previewLabel_ = new QLabel(this);
    previewLabel_->setAlignment(Qt::AlignCenter);
    previewLabel_->setMinimumSize(64, 64);
    previewLayout->addWidget(previewLabel_);

    previewDescLabel_ = new QLabel(this);
    previewDescLabel_->setWordWrap(true);
    previewDescLabel_->setAlignment(Qt::AlignCenter);
    previewLayout->addWidget(previewDescLabel_);

    previewLayout->addStretch();
    contentLayout->addWidget(previewGroup, 1);

    mainLayout->addLayout(contentLayout);

    // Button box
    auto* buttonLayout = new QHBoxLayout();
    clearButton_ = new QPushButton(tr("Clear"), this);
    clearButton_->setToolTip(tr("Remove current selection"));
    buttonLayout->addWidget(clearButton_);

    buttonLayout->addStretch();

    cancelButton_ = new QPushButton(tr("Cancel"), this);
    buttonLayout->addWidget(cancelButton_);

    selectButton_ = new QPushButton(tr("Select"), this);
    selectButton_->setDefault(true);
    selectButton_->setEnabled(false);
    buttonLayout->addWidget(selectButton_);

    mainLayout->addLayout(buttonLayout);

    // Connect signals
    connect(searchEdit_, &QLineEdit::textChanged,
        this, &FlagSelectorDialog::onSearchTextChanged);
    connect(listWidget_, &QListWidget::itemSelectionChanged,
        this, &FlagSelectorDialog::onItemSelectionChanged);
    connect(listWidget_, &QListWidget::itemDoubleClicked,
        this, &FlagSelectorDialog::onSelectClicked);
    connect(clearButton_, &QPushButton::clicked,
        this, &FlagSelectorDialog::onClearClicked);
    connect(selectButton_, &QPushButton::clicked,
        this, &FlagSelectorDialog::onSelectClicked);
    connect(cancelButton_, &QPushButton::clicked,
        this, &QDialog::reject);
}

void FlagSelectorDialog::onImageListLoaded() {
    BOOST_LOG_SEV(lg(), debug) << "Image list loaded, populating...";
    populateList();
}

void FlagSelectorDialog::populateList() {
    listWidget_->clear();

    const auto& images = imageCache_->availableImages();
    BOOST_LOG_SEV(lg(), debug) << "Populating list with " << images.size() << " images";

    for (const auto& img : images) {
        auto* item = new QListWidgetItem();
        item->setText(QString::fromStdString(img.key));
        item->setToolTip(QString::fromStdString(img.description));
        item->setData(Qt::UserRole, QString::fromStdString(img.image_id));
        item->setData(Qt::UserRole + 1, QString::fromStdString(img.description));

        // Set placeholder icon
        item->setIcon(QIcon(":/icons/ic_fluent_flag_20_regular.svg"));

        listWidget_->addItem(item);

        // Select current item if it matches
        if (QString::fromStdString(img.image_id) == currentImageId_) {
            item->setSelected(true);
            listWidget_->scrollToItem(item);
        }
    }

    // Load icons for visible items
    loadVisibleIcons();
}

void FlagSelectorDialog::filterList(const QString& filter) {
    for (int i = 0; i < listWidget_->count(); ++i) {
        auto* item = listWidget_->item(i);
        bool visible = filter.isEmpty() ||
            item->text().contains(filter, Qt::CaseInsensitive) ||
            item->data(Qt::UserRole + 1).toString().contains(filter, Qt::CaseInsensitive);
        item->setHidden(!visible);
    }
}

void FlagSelectorDialog::loadVisibleIcons() {
    // Load icons for the first few visible items
    int loaded = 0;
    for (int i = 0; i < listWidget_->count() && loaded < 20; ++i) {
        auto* item = listWidget_->item(i);
        if (item->isHidden())
            continue;

        QString image_id = item->data(Qt::UserRole).toString();
        QIcon icon = imageCache_->getImageIcon(image_id.toStdString());

        if (icon.isNull()) {
            // Request loading
            imageCache_->loadImageById(image_id.toStdString());
        } else {
            item->setIcon(icon);
        }
        ++loaded;
    }
}

void FlagSelectorDialog::onImageLoaded(const QString& image_id) {
    // Update the icon for the loaded image
    for (int i = 0; i < listWidget_->count(); ++i) {
        auto* item = listWidget_->item(i);
        if (item->data(Qt::UserRole).toString() == image_id) {
            QIcon icon = imageCache_->getImageIcon(image_id.toStdString());
            if (!icon.isNull()) {
                item->setIcon(icon);
            }
            break;
        }
    }

    // Update preview if the loaded image is currently selected
    auto selected = listWidget_->selectedItems();
    if (!selected.isEmpty() &&
        selected.first()->data(Qt::UserRole).toString() == image_id) {
        QIcon icon = imageCache_->getImageIcon(image_id.toStdString());
        if (!icon.isNull()) {
            previewLabel_->setPixmap(icon.pixmap(64, 64));
        }
    }
}

void FlagSelectorDialog::onSearchTextChanged(const QString& text) {
    filterList(text);
    loadVisibleIcons();
}

void FlagSelectorDialog::onItemSelectionChanged() {
    auto selected = listWidget_->selectedItems();
    if (selected.isEmpty()) {
        previewLabel_->clear();
        previewDescLabel_->clear();
        selectButton_->setEnabled(false);
        return;
    }

    auto* item = selected.first();
    QString image_id = item->data(Qt::UserRole).toString();
    QString description = item->data(Qt::UserRole + 1).toString();

    // Update preview
    QIcon icon = imageCache_->getImageIcon(image_id.toStdString());
    if (icon.isNull()) {
        previewLabel_->setText(tr("Loading..."));
        imageCache_->loadImageById(image_id.toStdString());
    } else {
        previewLabel_->setPixmap(icon.pixmap(64, 64));
    }
    previewDescLabel_->setText(description);

    selectButton_->setEnabled(true);
}

void FlagSelectorDialog::onClearClicked() {
    BOOST_LOG_SEV(lg(), debug) << "Clear clicked - removing flag association";
    selectedImageId_.clear();
    accept();
}

void FlagSelectorDialog::onSelectClicked() {
    auto selected = listWidget_->selectedItems();
    if (selected.isEmpty()) {
        return;
    }

    selectedImageId_ = selected.first()->data(Qt::UserRole).toString();
    BOOST_LOG_SEV(lg(), debug) << "Selected image: " << selectedImageId_.toStdString();
    accept();
}

}
